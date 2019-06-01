import logging
import sys

module_logger = logging.getLogger(__name__)


class BFState:

    def __init__(self):
        self._state = [0]*int(10)
        self._idx = 0

    def __getitem__(self, idx):
        return self._state[idx]

    @property
    def idx(self):
        return self._idx

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
    module_logger.debug(f"interpret: cmds={''.join(cmds)}, idx={state.idx}")
    def get_enclosed_cmds(cmds):

        idx = cmds[::-1].index("]")
        if idx != 0:
            cmds = cmds[:-idx-1]
        else:
            cmds = cmds[:-1]

        count = 0

        end = len(cmds) - 1
        for i, c in enumerate(cmds):
            if c == "[":
                count += 1
            elif c == "]":
                count -= 1
            if count == 0:
                end = i
                break
        return cmds[1:end+1]
    i = 0
    while i < len(cmds):
        cmd = cmds[i]
        module_logger.debug(
            (f"i={i}, cmd={cmd}, idx={state.idx}, "
             f"state={','.join([str(c) for c in state._state])}"))
        if cmd == ">":
            state.right()
        elif cmd == "<":
            state.left()
        elif cmd == "+":
            state.incr()
        elif cmd == "-":
            state.decr()
        elif cmd == ".":
            # module_logger.debug(f"state={','.join([str(c) for c in state._state])}")
            print(state, end="")
        elif cmd == "[":
            chunk = get_enclosed_cmds(cmds[i:])
            module_logger.debug(f"chunk={''.join(chunk)}")
            # module_logger.debug(f"state={''.join([str(c) for c in state._state])}")
            # idx = state.idx
            # while state[idx] > 0:
            while int(state) > 0:
                interpret(state, chunk)
                module_logger.debug(f"state={','.join([str(c) for c in state._state])}")
            i += len(chunk)
            module_logger.debug(f"after chunk, idx={state.idx}")
        elif cmd == "]":
            pass
        i += 1


def main():
    logging.basicConfig(level=logging.INFO)
    file_path = sys.argv[1]
    if file_path == "":
        raise RuntimeError("Please specify a file to interpret")

    with open(file_path, "r") as f:
        contents = f.read()

    state = BFState()
    bf_program_str = pre_process(contents)
    # module_logger.debug("".join(bf_program_str))
    interpret(state, bf_program_str)


if __name__ == "__main__":
    main()
