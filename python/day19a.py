#!/usr/bin/env python3

import copy

lines = []
with open("../input/day19.txt") as f:
    lines = f.read().splitlines()

scanner_points = {}
scanner = None
for line in lines:
    if line == "":
        continue
    if line.startswith("---"):
        scanner = int(line.split()[2])
        scanner_points[scanner] = []
        continue
    coord = tuple(map(int, line.split(",")))
    scanner_points[scanner].append(coord)

def mmult(l, r):
    result = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    for oy in range(3):
        for ox in range(3):
            for i in range(3):
                result[oy][ox] += l[oy][i] * r[i][ox]
    return result

def mvmult(m, v):
    result = [0, 0, 0]
    for oy in range(3):
        for i in range(3):
            result[oy] += m[oy][i] * v[i]
    return result

def vsub(v1, v2):
    return tuple(e1 - e2 for (e1, e2) in zip(v1, v2))

def vadd(v1, v2):
    return tuple(e1 + e2 for (e1, e2) in zip(v1, v2))

def gen_rotate_matrices():
    c90 = 0
    s90 = 1
    r1 = [
        [   1,    0,    0],
        [   0,  c90, -s90],
        [   0,  s90,  c90],
    ]
    r2 = [
        [ c90,    0,  s90],
        [   0,    1,    0],
        [-s90,    0,  c90],
    ]
    r3 = [
        [ c90, -s90,    0],
        [ s90,  c90,    0],
        [   0,    0,    1],
    ]
    matrices = set()
    m1 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    for x in range(4):
        m2 = copy.deepcopy(m1)
        for y in range(4):
            m3 = copy.deepcopy(m2)
            for z in range(4):
                matrices.add(tuple(tuple(c for c in r) for r in m3))
                m3 = mmult(r3, m3)
            m2 = mmult(r2, m2)
        m1 = mmult(r1, m1)
    return matrices

matrices = gen_rotate_matrices()
scanner_points_r = {
    scanner: [
        [mvmult(matrix, point) for point in points]
        for matrix in matrices
    ]
    for (scanner, points) in scanner_points.items()
}

scanner_pos = {}
abs_scanner_points = {}
def set_scanner_pos_rot(scanner, pos, rn):
    global scanner_pos
    global abs_scanner_points
    scanner_pos[scanner] = pos
    abs_scanner_points[scanner] = [vadd(pos, p) for p in scanner_points_r[scanner][rn]]
    del scanner_points_r[scanner]

# Arbitrary first choice
set_scanner_pos_rot(0, (0, 0, 0), 0)

def check(scanner1, scanner2, s2rn):
    deltas = {}
    for p1 in abs_scanner_points[scanner1]:
        for p2 in scanner_points_r[scanner2][s2rn]:
            delta = vsub(p1, p2)
            deltas[delta] = deltas.get(delta, 0) + 1
    for (delta, count) in deltas.items():
        if count >= 12:
            return delta
    return None

while len(scanner_points_r):
    changed = False
    for scanner1 in list(scanner_pos.keys()):
        for scanner2 in list(scanner_points_r.keys()):
            for s2rn in range(len(matrices)):
                scanner2_pos = check(scanner1, scanner2, s2rn)
                if scanner2_pos is not None:
                    set_scanner_pos_rot(scanner2, scanner2_pos, s2rn)
                    changed = True
                    break
    if not changed:
        raise Exception("Infinite loop")

points = set()
for (scanner, scanner_points) in abs_scanner_points.items():
    for scanner_point in scanner_points:
        points.add(scanner_point)
print(len(points))
