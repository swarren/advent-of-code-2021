#!/usr/bin/env python3

import functools

def cmd_fwd(state, arg):
    (aim, pos) = state
    (x, depth) = pos
    return (aim, (x + arg, depth + (arg * aim)))

def cmd_down(state, arg):
    (aim, pos) = state
    return (aim + arg, pos)

def cmd_up(state, arg):
    (aim, pos) = state
    return (aim - arg, pos)

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

state = functools.reduce(combine, data, (0, (0, 0)))
(aim, pos) = state
print(pos[0] * pos[1])
