import sys
import struct
import json
from subprocess import Popen, DEVNULL


def main():
    message = get_message()
    url = message.get("url")

    mpv_args = ["--no-terminal", "--pause", "--force-window=immediate"]

    args = ["mpv", *mpv_args, "--", url]
    Popen(args, stdin=DEVNULL, stdout=DEVNULL, stderr=DEVNULL)
    # Need to respond something to avoid "Error: An unexpected error occurred"
    # in Browser Console.
    send_message("ok")


def get_message():
    raw_length = sys.stdin.buffer.read(4)
    if not raw_length:
        return {}
    length = struct.unpack("@I", raw_length)[0]
    message = sys.stdin.buffer.read(length).decode("utf-8")
    return json.loads(message)


def send_message(message):
    # https://stackoverflow.com/a/56563264
    # https://docs.python.org/3/library/json.html#basic-usage
    # To get the most compact JSON representation, you should specify
    # (',', ':') to eliminate whitespace.
    content = json.dumps(message, separators=(",", ":")).encode("utf-8")
    length = struct.pack("@I", len(content))
    sys.stdout.buffer.write(length)
    sys.stdout.buffer.write(content)
    sys.stdout.buffer.flush()


if __name__ == "__main__":
    main()
