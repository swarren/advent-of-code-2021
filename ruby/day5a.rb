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
        ymin = [y0, y1].min
        ymax = [y0, y1].max
        for y in ymin..ymax
            c = [x0, y]
            floor[c] += 1
        end
    elsif y0 == y1
        xmin = [x0, x1].min
        xmax = [x0, x1].max
        for x in xmin..xmax
            c = [x, y0]
            floor[c] += 1
        end
    end
}
print floor.values.filter { |count| count > 1 }.length, "\n"

