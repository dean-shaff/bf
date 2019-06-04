class State:

    def __init__(self, x, right=None, left=None):
        self.x = x
        self._right = right
        self._left = left

    @property
    def left(self):
        return self._left

    @property
    def right(self):
        return self._right

    def __str__(self) -> str:
        return str(self.x)


def insert(val, right, left=None):
    if right is None:
        return State(val, left=left)
    else:
        return State(
            right.x,
            right=insert(val, right.right, right),
            left=left
        )


if __name__ == "__main__":

    l = list(range(10))
    state = State(l[0])
    for i in l[1:]:
        state = insert(i, state)
    print(state, state.left)
    while state.right is not None:
        state = state.right
        print(state, state.left, state.left.left)
