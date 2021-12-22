#!/usr/bin/env python3

import functools

lines = []
with open("../input/day3.txt") as f:
    lines = f.read().splitlines()

def filter_lines(lines, want_one_is_most):
    line_len = len(lines[0])
    for pos in range(line_len):
        if len(lines) == 1:
            return lines
        new_lines = {'0': [], '1': []}
        for line in lines:
            new_lines[line[pos]].append(line)
        one_is_most = len(new_lines['1']) >= len(new_lines['0'])
        if one_is_most == want_one_is_most:
            want = '1'
        else:
            want = '0'
        lines = new_lines[want]
    return lines

def rating(lines, want_one_is_most):
    lines = filter_lines(lines, want_one_is_most)
    return int(lines[0], 2)

o2_rating = rating(lines, True)
co2_rating = rating(lines, False)
print(o2_rating * co2_rating)
