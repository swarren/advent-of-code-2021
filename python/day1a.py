#!/usr/bin/env python3

import itertools

def pairs(xs):
    prev = None
    for x in xs:
        if prev is not None:
            yield(prev, x)
        prev = x

with open("../input/day1.txt") as f:
    data = map(int, f.read().splitlines())

increases = [1 for a, b in pairs(data) if b > a]
count = sum(increases)
print(count)
