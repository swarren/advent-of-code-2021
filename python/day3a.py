#!/usr/bin/env python3

import functools

lines = []
with open("../input/day3.txt") as f:
    lines = f.read().splitlines()

line_count = len(lines)
num_bits = len(lines[0])
bit_vals_by_line = map(lambda x: map(int, x), lines)

def accumulate_bit_counts(bit_counts_by_bit, bit_vals):
    return tuple(map(sum, zip(bit_counts_by_bit, bit_vals)))
bit_counts_by_bit_zero = [0] * num_bits
bit_counts_by_bit = functools.reduce(accumulate_bit_counts, bit_vals_by_line, bit_counts_by_bit_zero)

def is_most_popular_as_int(bit_count):
    return int(bit_count >= line_count / 2)
gamma_bits = map(is_most_popular_as_int, bit_counts_by_bit)

bit_pos_values = [2**x for x in range(num_bits -1, -1, -1)]
gamma = sum(map(lambda x: x[0] * x[1], zip(gamma_bits, bit_pos_values)))

epsilon = (~gamma) & ((1 << num_bits) - 1)

power = gamma * epsilon
print(power)
