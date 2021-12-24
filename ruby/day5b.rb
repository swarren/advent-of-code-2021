#!/usr/bin/ruby

lines = File.readlines('../input/day5.txt')

vent_lines = []
lines.each {
    |line|
    coords = line.split(/,| -> /).map { |x| x.to_i() }
    vent_lines.append(coords)
}

floor = {}
floor.default = 0
vent_lines.each {
    |vent_line|
    x0, y0, x1, y1 = vent_line
    if x0 == x1
        xinc = 0
    elsif x0 < x1
        xinc = 1
    else
        xinc = -1
    end
    if y0 == y1
        yinc = 0
    elsif y0 < y1
        yinc = 1
    else
        yinc = -1
    end
    x = x0
    y = y0
    while (x != x1) or (y != y1)
        c = [x, y]
        floor[c] += 1
        x += xinc
        y += yinc
    end
    c = [x1, y1]
    floor[c] += 1
}
print floor.values.filter { |count| count > 1 }.length, "\n"

