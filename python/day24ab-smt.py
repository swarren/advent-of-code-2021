#!/usr/bin/env python3

import z3

instructions = []
with open("../input/day24.txt", "rt") as f:
    instructions = f.read().splitlines()

digits = len(tuple(i for i in instructions if i.startswith("inp")))
digitVars = [z3.Int('digit' + str(digit)) for digit in range(digits)]

cpuState = {
    "x": z3.IntSort().cast(0),
    "y": z3.IntSort().cast(0),
    "z": z3.IntSort().cast(0),
    "w": z3.IntSort().cast(0),
}

def evalRhs(rhs):
    val = cpuState.get(rhs, None)
    if val is None:
        val = z3.IntSort().cast(int(rhs))
    return val

digit = 0
for i in instructions:
    fields = i.split()
    if fields[0] == "inp":
        cpuState[fields[1]] = digitVars[digit]
        digit += 1
    elif fields[0] == "add":
        rhs = evalRhs(fields[2])
        cpuState[fields[1]] = cpuState[fields[1]] + rhs
    elif fields[0] == "mul":
        rhs = evalRhs(fields[2])
        cpuState[fields[1]] = cpuState[fields[1]] * rhs
    elif fields[0] == "div":
        rhs = evalRhs(fields[2])
        cpuState[fields[1]] = z3.IntSort().cast(cpuState[fields[1]] / rhs)
    elif fields[0] == "mod":
        rhs = evalRhs(fields[2])
        cpuState[fields[1]] = cpuState[fields[1]] % rhs
    elif fields[0] == "eql":
        rhs = evalRhs(fields[2])
        cpuState[fields[1]] = z3.IntSort().cast(cpuState[fields[1]] == rhs)
    else:
        raise Exception("???")

s = z3.Solver()
for dv in digitVars:
    s.add(dv >= 1)
    s.add(dv <= 9)
s.add(cpuState["z"] == 0)
solutions = []
while True:
    result = s.check()
    if result != z3.sat:
        break
    m = s.model()
    solution = 0
    constraints = []
    for dv in digitVars:
        solution *= 10
        digitVal = m[dv].as_long()
        solution += digitVal
        constraints.append(dv != digitVal)
    solutions.append(solution)
    s.add(z3.Or(constraints))
print("min:", min(solutions), "max:", max(solutions))
