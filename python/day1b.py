#!/usr/bin/env python3

import collections
import itertools

with open("../input/day1.txt") as f:
    data = map(int, f.read().splitlines())

def windows(xs, n):
    it = iter(xs)
    window = collections.deque(itertools.islice(it, n), maxlen=n)
    if len(window) == n:
        yield tuple(window)
    for x in it:
        window.append(x)
        yield tuple(window)

ws = windows(data, 3)
wsums = map(sum, ws)
increases = [1 for a, b in windows(wsums, 2) if b > a]
count = sum(increases)
print(count)
