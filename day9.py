#!/usr/bin/env python3
from collections import deque


def run_game(players, amount):
    circle = deque([0])
    scores = [0 for _ in range(players)]

    for marble in range(1, amount + 1):
        if marble % 23:
            circle.rotate(-1)
            circle.append(marble)
        else:
            circle.rotate(7)
            scores[marble % players] += marble + circle.pop()
            circle.rotate(-1)
    return scores


print(max(run_game(435, 71184)))
print(max(run_game(435, 71184 * 100)))
