#!/usr/bin/env python3

import functools

def cmd_fwd(pos, arg):
    (x, depth) = pos
    return (x + arg, depth)

def cmd_down(pos, arg):
    (x, depth) = pos
    return (x, depth + arg)

def cmd_up(pos, arg):
    (x, depth) = pos
    return (x, depth - arg)

funcs = {
    'forward': cmd_fwd,
    'down': cmd_down,
    'up': cmd_up,
}

data = []
with open("../input/day2.txt") as f:
    lines = f.read().splitlines()
    for line in lines:
        (cmds, args) = line.split()
        cmdf = funcs[cmds]
        argi = int(args)
        data.append((cmdf, argi))

def combine(pos, op):
    (f, arg) = op
    return f(pos, arg)

pos = functools.reduce(combine, data, (0, 0))
print(pos[0] * pos[1])
