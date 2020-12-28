import pytest  # type: ignore

from io import StringIO
from itertools import groupby
from typing import Set, Tuple, Iterable, Callable, Any

from hypothesis import assume, given  # type: ignore
import hypothesis.strategies as st  # type: ignore

from patch import Compost

MAX_OFFSET = 1000
ints = st.integers(min_value=0, max_value=MAX_OFFSET)


@given(ranges=st.iterables(st.tuples(ints, ints)))
def test_compost_free(ranges: Iterable[Tuple[int, int]]) -> None:
    compost = Compost()

    # Use a completely different (however inefficient) implementation here to
    # check for overlaps so that we can make sure the actual implementation
    # isn't "accidentally correct".
    bitset: Set[int] = set()
    for start, end in ranges:
        if end < start:
            with pytest.raises(ValueError):
                compost.free(start, end)
        else:
            must_fail = False
            new_bitset = set()
            for bit in range(start, end + 1):
                if bit in bitset:
                    must_fail = True
                new_bitset.add(bit)
            if must_fail:
                with pytest.raises(ValueError):
                    compost.free(start, end)
            else:
                bitset |= new_bitset
                compost.free(start, end)

    # Allocating from the biggest chunks to the smallest ones needs to always
    # work and will also make sure that contiguous areas are properly merged.
    grouped = groupby(range(MAX_OFFSET + 1), bitset.__contains__)
    chunks = [len(list(chunk)) for valid, chunk in grouped if valid]
    for size in sorted(chunks, reverse=True):
        compost.alloc(size)

    # The last allocation is just one byte and it must fail, since we exhausted
    # all the space we have on the compost heap.
    with pytest.raises(LookupError):
        compost.alloc(1)


@st.composite
def valid_ranges(
    draw: Callable[..., Any],
    min_chunksize: int = 1,
    max_chunksize: int = 10000,
    min_ranges: int = 0,
    max_ranges: int = 100,
    allow_gaps: bool = True,
):
    gap = st.tuples(st.just(False), st.integers(min_value=0))
    occupied = st.tuples(st.just(True),
                         st.integers(min_value=min_chunksize,
                                     max_value=max_chunksize))
    elems = st.one_of(gap, occupied) if allow_gaps else occupied
    sizes = draw(st.iterables(elems, min_size=min_ranges, max_size=max_ranges))
    ranges = []
    offset = 0
    for is_occupied, size in sizes:
        if is_occupied:
            ranges.append((offset, offset + size))
            offset += 1
        offset += size
    return ranges


@given(ranges=valid_ranges())
def test_compost_sizes(ranges: Iterable[Tuple[int, int]]) -> None:
    compost = Compost()
    total = 0
    smallest = 0
    largest = 0
    for start, end in ranges:
        size = end - start + 1
        if smallest == 0 or size < smallest:
            smallest = size
        if size > largest:
            largest = size
        total += size
        compost.free(start, end)

    assert compost.available == total

    # Values could have been merged, so the sizes will be at *least* the value
    # we calculated here in the naive way.
    assert compost.smallest_chunk >= smallest
    assert compost.largest_chunk >= largest


@given(ranges=valid_ranges(allow_gaps=False, min_ranges=1))
def test_compost_gapless_alloc(ranges: Iterable[Tuple[int, int]]) -> None:
    compost = Compost()
    for start, end in ranges:
        compost.free(start, end)

    assert compost.available > 0
    compost.alloc(compost.available)
    assert compost.available == 0


@given(ranges=valid_ranges(max_chunksize=100, min_ranges=1, max_ranges=10))
def test_compost_alloc_nofrag(ranges: Iterable[Tuple[int, int]]) -> None:
    compost = Compost()
    for start, end in ranges:
        compost.free(start, end)

    total = compost.available
    for n in range(compost.available):
        compost.alloc(1)
        total -= 1
        assert compost.available == total

    with pytest.raises(LookupError):
        compost.alloc(1)


@given(ranges=valid_ranges(min_chunksize=4, max_chunksize=20, min_ranges=1),
       allocs=st.iterables(st.integers(min_value=1, max_value=21), min_size=1))
def test_compost_alloc(ranges: Iterable[Tuple[int, int]],
                       allocs: Iterable[int]) -> None:
    compost = Compost()
    for start, end in ranges:
        compost.free(start, end)

    for alloc_size in allocs:
        assume(compost.largest_chunk >= alloc_size)
        size_before = compost.available
        compost.alloc(alloc_size)
        assert size_before - alloc_size == compost.available


@given(ranges=valid_ranges(min_chunksize=2, max_chunksize=5,
                           min_ranges=10, max_ranges=15,
                           allow_gaps=False),
       use=st.integers(min_value=-1, max_value=21))
def test_compost_maybe_alloc(ranges: Iterable[Tuple[int, int]],
                             use: int) -> None:
    compost = Compost()
    for start, end in ranges:
        compost.free(start, end)

    # This is similar to the one in test_compost_free and it serves as a canary
    # to make sure we don't accidentally overwrite something we don't want.
    bitset_before = {i for start, end in compost.offsets
                     for i in range(1000) if start <= i <= end}

    initial_available = compost.available
    with compost.maybe_alloc(20) as (_, mark_used):
        assert initial_available - compost.available == 20
        assume(0 <= use <= 20)
        mark_used(use)

    # If the bitset before the allocation is a superset, something is clearly
    # wrong and we're now in the middle of invalid memory.
    bitset_afterwards = {i for start, end in compost.offsets
                         for i in range(1000) if start <= i <= end}
    assert bitset_before.issuperset(bitset_afterwards)

    assert initial_available - use == compost.available


@given(start=st.integers(min_value=0, max_value=100),
       alloc_sizes=st.iterables(st.integers(min_value=1, max_value=20),
                                max_size=5))
def test_compost_writes(start: int, alloc_sizes: Iterable[int]):
    compost = Compost()
    compost.free(start, start + 100)

    io = StringIO('.' * 200)
    alloc_chars = 'abcdefghijkl'

    expected_written = ''
    for n, asize in enumerate(alloc_sizes):
        char = alloc_chars[n]
        offset = compost.alloc(asize)
        io.seek(offset)
        expected_written += char * asize
        io.write(char * asize)

    io.seek(0)
    assert io.read().startswith('.' * start + expected_written)


def test_invalid_compost_calls():
    compost = Compost()

    with pytest.raises(ValueError):
        compost.free(10, 9)

    with pytest.raises(ValueError):
        compost.free(-10, 0)

    with pytest.raises(ValueError):
        compost.alloc(0)

    with pytest.raises(ValueError):
        compost.alloc(-10)
