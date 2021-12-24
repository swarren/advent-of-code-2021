#!/usr/bin/ruby

lines = File.readlines('../input/day6.txt')

fish_per_age = Array.new(9, 0)
init_ages = lines[0].split(",").map { |x| x.to_i() }
init_ages.each { |age| fish_per_age[age] += 1 }

(1..80).each {
    zeros = fish_per_age[0]
    fish_per_age = fish_per_age[1...]
    fish_per_age[6] += zeros
    fish_per_age[8] = zeros
}
print fish_per_age.sum, "\n"
