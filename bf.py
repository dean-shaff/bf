import logging
import sys

module_logger = logging.getLogger(__name__)


class BFState:

    def __init__(self):
        self._state = [0]*int(100)
        self._idx = 0

    def __getitem__(self, idx):
        return self._state[idx]

    @property
    def idx(self):
        return self._idx

    @property
    def state(self) -> str:
        return ",".join([str(c) for c in self._state])

    def right(self):
        self._idx += 1
        return self

    def left(self):
        self._idx -= 1
        if self._idx < 0:
            raise RuntimeError("Can't go below zero!")
        return self

    def incr(self):
        self._state[self._idx] += 1
        return self

    def decr(self):
        self._state[self._idx] -= 1
        return self

    def __int__(self):
        return self._state[self._idx]

    def __str__(self):
        return str(chr(self._state[self._idx]))


_bf_cmds = [">", "<", "+", "-", ".", ",", "[", "]"]


def pre_process(bf_str: str):
    """
    pre-process a string representing a fb program,
    ridding it of any characters that aren't command characters
    """
    return [c for c in bf_str if c in _bf_cmds]


def interpret(state, cmds):

    module_logger.debug((f"interpret: len(cmds)={len(cmds)}, "
                         f"cmds={''.join(cmds)}, idx={state.idx}"))

    def _get_enclosed_cmds(cmds):

        def _get_matching_bracket(count, end, cmds):
            if len(cmds) == 0:
                return count, end
            if count == 0:
                return count, end

            cmd = cmds[0]
            new_count = count
            if cmd == "[":
                new_count += 1
            elif cmd == "]":
                new_count -= 1

            return _get_matching_bracket(
                new_count, end+1, cmds[1:])

        count, end = _get_matching_bracket(1, 0, cmds[1:])

        return cmds[1:end+1]

    if len(cmds) == 0:
        return
    cmd = cmds[0]
    idx = 1
    module_logger.debug(
        (f"cmd={cmd}, idx={state.idx}, state={state.state}"))
    if cmd == ">":
        state.right()
    elif cmd == "<":
        state.left()
    elif cmd == "+":
        state.incr()
    elif cmd == "-":
        state.decr()
    elif cmd == ".":
        print(state, end="")
    elif cmd == "[":
        chunk = _get_enclosed_cmds(cmds)
        module_logger.debug(f"chunk={''.join(chunk)}")
        while int(state) > 0:
            interpret(state, chunk)
            module_logger.debug(f"state={state.state}")
        module_logger.debug(f"after chunk, idx={state.idx}")
        idx = len(chunk) + 1
    elif cmd == "]":
        pass
    interpret(state, cmds[idx:])


def main():
    logging.basicConfig(level=logging.INFO)
    file_path = sys.argv[1]
    if file_path == "":
        raise RuntimeError("Please specify a file to interpret")

    with open(file_path, "r") as f:
        contents = f.read()

    state = BFState()
    bf_program_str = pre_process(contents)
    interpret(state, bf_program_str)


if __name__ == "__main__":
    main()
