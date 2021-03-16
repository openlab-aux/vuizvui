import os
import sys
import logging

from contextlib import contextmanager
from itertools import dropwhile
from pathlib import Path
from typing import Dict, List, Tuple, Any, Optional, ContextManager, \
                   Generator, Callable
from r2pipe import open as R2Pipe  # type: ignore


class Compost:
    """
    Represents non-overlapping areas of the binary that can be re-used
    to store additional data and code. In a high level way, this is a similar
    interface to 'malloc' and 'free', where we use free() to dispose something
    and alloc() to get something from the (compost) heap.
    """
    def __init__(self):
        self.offsets: List[Tuple[int, int]] = []

    @property
    def available(self) -> int:
        """
        The total amount of data available for re-use, not necessarily
        contiguous.
        """
        return sum(end + 1 - start for start, end in self.offsets)

    @property
    def largest_chunk(self) -> int:
        """
        The largest chunk of contiguous amount data that is avaliable for
        re-use.
        """
        if self.offsets:
            return self.offsets[-1][1] - self.offsets[-1][0] + 1
        return 0

    @property
    def smallest_chunk(self) -> int:
        """
        The smallest chunk of contiguous amount data that is avaliable for
        re-use.
        """
        if self.offsets:
            return self.offsets[0][1] - self.offsets[0][0] + 1
        return 0

    def free(self, start: int, end: int) -> None:
        """
        Add the given byte range to the space that can be reused later.
        """
        if start < 0:
            raise ValueError("Start offset needs to positive.")
        if end < start:
            msg = "End offset needs to be larger than start offset."
            raise ValueError(msg)

        to_delete = []
        for n, existing in enumerate(self.offsets):
            existing_start, existing_end = existing

            if start <= existing_end and existing_start <= end:
                wanted = start, end
                msg = f"Range {wanted!r} is overlapping with {existing!r}."
                raise ValueError(msg)

            # If the offsets follow or precede existing ranges, merge them.
            if existing_end + 1 == start:
                start = existing_start
                to_delete.append(n)
            if end + 1 == existing_start:
                end = existing_end
                to_delete.append(n)
        for n in to_delete:
            del self.offsets[n]

        self.offsets.append((start, end))
        self.offsets.sort(key=lambda o: o[1] - o[0])

    def alloc(self, size: int) -> int:
        """
        Get `size` bytes of data for re-use, mark them as used and return its
        offset.
        """
        if size < 1:
            raise ValueError("Allocation size has to be at least 1 byte.")
        for n, (start, end) in enumerate(self.offsets):
            if end - start + 1 >= size:
                new_start = start + size
                self.offsets[n] = (new_start, end)
                if new_start == end + 1:
                    del self.offsets[n]
                return start
        if size == 1:
            msg = f"Unable to find space for one byte in {self.offsets!r}."
        else:
            msg = f"Unable to find space for {{}} bytes in {self.offsets!r}."
        raise LookupError(msg.format(size))

    @contextmanager
    def maybe_alloc(
        self, size: int
    ) -> Generator[Tuple[int, Callable[[int], None]], None, None]:
        """
        Allocate at least `size` bytes and pass the offset and a callback to
        the context. The offset is the start address of the allocated bytes and
        the callback accepts a single argument denoting how many bytes were
        actually used. After leaving the context, the excess bytes are freed
        back to the compost.
        """
        offset = self.alloc(size)
        used = 0

        def _mark_used(used_size: int):
            nonlocal used
            if used_size < 0 or used_size > size:
                raise ValueError(f"Tried to mark {used_size} bytes as used,"
                                 f" but only {size} bytes are available.")
            used = used_size

        try:
            yield offset, _mark_used
        finally:
            if used < size:
                start = offset + used
                self.free(start, offset + size - 1)


class Patcher(ContextManager['Patcher']):
    compost: Compost

    def __init__(self, filename: str):
        self.filename = filename
        self.__r2: Optional[R2Pipe] = None
        self.__addr: Optional[int] = None
        self.compost = Compost()

    def __enter__(self) -> 'Patcher':
        self.__r2 = R2Pipe(self.filename, ['-w'])
        logging.info(f'Analysing program {self.filename}.')
        self.raw_command('aa')
        return self

    def __exit__(self, *exc_details) -> None:
        assert self.__r2 is not None
        self.__r2.quit()

    def raw_command(self, cmd: str) -> str:
        assert self.__r2 is not None
        return self.__r2.cmd(cmd)

    def raw_command_json(self, cmd: str) -> Any:
        assert self.__r2 is not None
        return self.__r2.cmdj(cmd)

    @property
    def address(self) -> int:
        if self.__addr is None:
            self.__addr = self.raw_command_json('?vi $$')
        return self.__addr

    @address.setter
    def address(self, addr: int) -> None:
        self.raw_command(f's {addr}')
        self.__addr = addr

    def __invalidate_address(self):
        self.__addr = None

    @contextmanager
    def at_address(self, addr: int) -> Generator[None, None, None]:
        """
        Seek to given address and restore position after leaving context.
        """
        oldaddr = self.address
        self.address = addr
        try:
            yield
        finally:
            self.address = oldaddr

    def get_current_opcode_size(self, count: int = 1) -> int:
        """
        Return the size in bytes for the next `count` opcodes.
        """
        return self.raw_command_json(f'aos {count}')

    def get_assembled_size(self, *opcodes: str) -> int:
        """
        Return the size in bytes of the given opcodes. Note that JMP locations
        may be position-dependent, so you have to seek to the address first if
        you want this to be reliable.
        """
        result = self.raw_command(f'"wa* {";".join(opcodes)}"')
        assert result.startswith('wx ')
        return len(result[3:].strip()) >> 1

    def write_code(self, *opcodes: str) -> int:
        """
        Write the specified opcodes and return the written length in bytes.
        """
        oldaddr = self.address
        logging.info(f'Writing assembly of {opcodes!r} to address'
                     f' 0x{oldaddr:x}.')
        self.raw_command(f'"wa {";".join(opcodes)}"')
        self.raw_command(f'so {len(opcodes)}')
        self.__invalidate_address()
        return self.address - oldaddr

    def replace_code(self, *opcodes: str, count: int = 1) -> int:
        """
        Write the specified opcodes over the next `count` opcodes at the
        current position with padding and return the written length in bytes.
        """
        current_size = self.get_current_opcode_size(count)
        expected_size = self.get_assembled_size(*opcodes)
        assert expected_size <= current_size
        padsize = current_size - expected_size
        nops = ['nop'] * padsize
        logging.info(f'Replacing {count} opcodes ({current_size} bytes)'
                     f' at address 0x{self.address} with assembly'
                     f' {opcodes!r} (padded with {padsize} nops).')
        written = self.write_code(*opcodes, *nops)
        assert written == current_size, \
            f"Wanted to replace {count} opcodes consisting of {current_size}" \
            f" bytes, however we've written {written} bytes instead."
        return written

    def write_code_to_compost(self, *opcodes: str) -> int:
        """
        Write the specified opcodes to the `Compost` area and return the
        offset.
        """
        estimated = self.get_assembled_size(*opcodes)
        with self.compost.maybe_alloc(estimated + 20) as (offset, mark_used):
            with self.at_address(offset):
                logging.info(f'Writing assembly {opcodes!r} to compost at'
                             f' address 0x{offset:x}.')
                size = self.get_assembled_size(*opcodes)
                written = self.write_code(*opcodes)
                mark_used(written)
                assert written == size, \
                    f"Assembled size for {opcodes!r} is {size} bytes," \
                    f" but the actual code size is {written} bytes."
            return offset

    def write_cstring(self, value: str) -> int:
        """
        Write the specified C-String and return the length in bytes (which
        includes the NUL byte).
        """
        logging.info(f'Writing C-String {value!r} to address'
                     f' 0x{self.address:x}.')
        self.raw_command(f'w {value}\\0')
        size = len(value) + 1
        self.raw_command(f's+ {size}')
        self.address += size
        return size

    def write_cstring_to_compost(self, value: str) -> int:
        """
        Write the specified C-String to the `Compost` area and return its
        offset.
        """
        size = len(value) + 1
        offset = self.compost.alloc(size)
        with self.at_address(offset):
            logging.info(f'Writing C-String {value!r} to compost at'
                         f' address 0x{offset:x}.')
            self.raw_command(f'w {value}\\0')
        return offset

    def read_cstring(self, addr: int) -> Dict[Any, Any]:
        """
        Read the C-String at the specified `addr` and return its information.
        """
        return self.raw_command_json(f'pszj @{addr}')

    def load_function(self, name: str) -> Dict[Any, Any]:
        self.raw_command(f'sf {name}')
        fun = self.raw_command_json('pdfj')
        assert fun['name'] == name
        self.__invalidate_address()
        return fun

    def skip_opcodes(self, amount: int = 1) -> int:
        "Skip the specified amount of opcodes and return the total length."
        oldaddr = self.address
        self.raw_command(f'so {amount}')
        self.__invalidate_address()
        return self.address - oldaddr


def patch_get_executable_name(patcher: Patcher, out: Path) -> None:
    # We'll use the get_executable_name() function to not only hardcode the
    # executable path but also use the unused part for static data referenced
    # by other functions.
    fun = patcher.load_function('sym.get_executable_name')
    offset = fun["ops"][0]["offset"]

    # Let's just assume 32 bytes, which is enough to hopefully fit in the code.
    executable_offset = offset + 32

    # This makes sure that get_executable_name always returns the binary at
    # $out/bin/factorio, since we don't need to do all kinds of shenanigans at
    # runtime when using Nix.
    size = patcher.write_code(
        f'mov edi, {executable_offset}',
        'call sym.al_create_path',
        'ret',
    )
    assert size < 32, "Replacement code for get_executable_name " \
                      f"would overflow into compost area ({size} >= 32)."

    executable = str(out / 'bin' / 'factorio')
    patcher.address = executable_offset
    size = patcher.write_cstring(executable)

    # TODO: End should be the *real* end, not the offset of the last opcode!
    patcher.compost.free(executable_offset + size, fun["ops"][-1]["offset"])


def patch_read_data_path(patcher: Patcher, out: Path) -> None:
    data_path = str(out / 'share' / 'factorio')
    data_path_offset = patcher.write_cstring_to_compost(data_path)

    if patcher.raw_command_json('?V0') >= 5:
        name = 'str._usr_share_factorio'
    else:
        name = 'str.usr_share_factorio'
    matches = patcher.raw_command_json(f'/vj {name}')
    assert len(matches) > 0, \
        f'no matches found for /usr/share/factorio: {matches}'

    for match in matches:
        assert match['type'] == 'hexpair'
        assert len(match['data']) == 8
        patcher.raw_command(f'wv4 {data_path_offset} @{match["offset"]}')


def patch_write_data_path(patcher: Patcher) -> None:
    # This is the function which returns fs::path("$HOME/.factorio"), but since
    # we want to conform to XDG, we need to rewrite that function.
    if patcher.raw_command_json('?V0') >= 5:
        name = 'method.Paths.getSystemWriteData__'
    else:
        name = 'method.Paths.getSystemWriteData'
    fun = patcher.load_function(name)

    # The function in question uses getpwuid(getuid())->pw_dir, so let's find
    # out the register that references the value of the home directory.
    pwuid_call = 'call sym.imp.getpwuid'
    rest = dropwhile(lambda i: i['disasm'] != pwuid_call, fun['ops'])
    rest = dropwhile(lambda i: i['esil'].split(',')[1:2] != ['rax'], rest)
    home_register = next(rest)['esil'].split(',')[4]

    # Just to make sure we inspect this on the next upstream update if the
    # register has changed.
    assert home_register == 'r13'

    # Get all the data references of the current function so that we can not
    # only re-use them but also have a hint on what we might want to rewrite.
    refs = {}
    for n, op in enumerate(fun["ops"]):
        for ref in op.get('refs', []):
            if ref['type'] != 'DATA':
                continue
            refs[n] = patcher.read_cstring(ref['addr'])

    # The disassembly of the operation we want to rewrite.
    rewrite_pos_info = next(
        fun['ops'][n] for n, i in refs.items() if i['string'] == '/.factorio'
    )

    # We want to make sure that RSI contains "/.factorio", since that's the
    # register we want to rewrite.
    assert rewrite_pos_info['esil'] == f'{rewrite_pos_info["ptr"]},rsi,='

    # Seek to the rewrite position and memoize the address that comes directly
    # afterwards, so that we can JMP back accordingly.
    patcher.address = rewrite_pos_info["offset"]
    patcher.skip_opcodes()
    continuation_addr = patcher.address

    # After we're done, we need to restore these registers to the state before
    # we JMPed into our new code. We *only* restore registers for argument 1,
    # 4, 5, 6, 7, 8 and the integer return register because the second argument
    # is one that we actually *want* to override.
    restore_registers = ['rdi', 'rcx', 'r8', 'r9', 'r10', 'r11', 'rax']

    # Just to make sure we don't accidentally revert a register we write into.
    assert home_register not in restore_registers
    init_code = [f'push {reg}' for reg in restore_registers]
    cleanup_code = [f'pop {reg}' for reg in reversed(restore_registers)]
    cleanup_code.append(f'jmp {continuation_addr}')

    xdg_fallback = '/.local/share/factorio'
    xdg_fallback_offset = patcher.write_cstring_to_compost(xdg_fallback)

    # This code is the handler for whenever no XDG_DATA_HOME is defined.
    noxdg_code_offset = patcher.write_code_to_compost(
        f'mov rsi, {xdg_fallback_offset}',
        f'mov rdx, {xdg_fallback_offset + len(xdg_fallback)}',
        *cleanup_code,
    )

    xdg_data_home_offset = patcher.write_cstring_to_compost('XDG_DATA_HOME')

    factorio_subpath = '/factorio'
    factorio_subpath_offset = \
        patcher.write_cstring_to_compost(factorio_subpath)

    xdg_code_offset = patcher.write_code_to_compost(
        *init_code,

        f'mov rdi, {xdg_data_home_offset}',
        'call sym.imp.getenv',

        # If getenv returns NULL, let's jump to the fallback code (using
        # ~/.local/share) we've written previously.
        'test rax, rax',
        f'je {noxdg_code_offset}',

        # Essentially override the home directory with XDG_DATA_HOME and
        # replace "/.factorio" with "/factorio".
        f'mov {home_register}, rax',
        f'mov rsi, {factorio_subpath_offset}',
        f'mov rdx, {factorio_subpath_offset + len(factorio_subpath)}',

        *cleanup_code,
    )

    # Finally, let's write the initial JMP back to our handler code.
    patcher.address = rewrite_pos_info["offset"]
    patcher.write_code(f'jmp {xdg_code_offset}')


if __name__ == '__main__':
    logging.basicConfig(format='%(levelname)s:%(message)s', level=logging.INFO)

    with Patcher(sys.argv[1]) as patcher:
        out = Path(os.environ['out'])
        patch_get_executable_name(patcher, out)
        patch_read_data_path(patcher, out)
        patch_write_data_path(patcher)
