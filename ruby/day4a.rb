#!/usr/bin/ruby

lines = File.readlines('../input/day4.txt')

draws = lines[0].split(",").map { |draw| draw.to_i() }
boards = []
lines[1..].each_slice(6) {
    |boardlines| boards.append(boardlines[1..].map {
        |line| line.split().map {
            |val| val.to_i()
        }
    })
}

draws.each {
    |draw|
    boards.each {
        |board|
        board.each {
            |row|
            row.each_with_index {
                |val, i|
                if val == draw
                    row[i] = 0
                end
            }
        }
        row_sums = board.map { |row| row.sum }
        col_sums = board.transpose().map { |col| col.sum }
        if (row_sums.include? 0) or (col_sums.include? 0)
            score = row_sums.sum * draw
            print score, "\n"
            exit
        end
    }
}
