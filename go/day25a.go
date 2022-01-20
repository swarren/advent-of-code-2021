package main

import (
	"bufio"
	"fmt"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func readParseInput(fn string) [][]rune {
	f, err := os.Open(fn)
	check(err)
	defer f.Close()

	grid := [][]rune{}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		sLine := scanner.Text()
		aLine := []rune(sLine)
		grid = append(grid, aLine)
	}

	return grid
}

func moveHerd(grid [][]rune, cucumberToMove rune, xo, yo int) ([][]rune, bool) {
	nRows := len(grid)
	nCols := len(grid[0])
	anyMoved := false
	nextGrid := [][]rune{}
	for _, row := range grid {
		rowCopy := append(make([]rune, 0, nCols), row...)
		nextGrid = append(nextGrid, rowCopy)
	}
	for iRow, row := range grid {
		for iCol, cell := range row {
			if cell == cucumberToMove {
				iRowTo := (iRow + yo) % nRows
				iColTo := (iCol + xo) % nCols
				if grid[iRowTo][iColTo] == '.' {
					nextGrid[iRowTo][iColTo] = cell
					nextGrid[iRow][iCol] = '.'
					anyMoved = true
				}
			}
		}
	}
	return nextGrid, anyMoved
}

func step(grid [][]rune) ([][]rune, bool) {
	gridAfterMovedEast, anyMovedEast := moveHerd(grid, '>', 1, 0)
	gridAfterMovedSouth, anyMovedSouth := moveHerd(gridAfterMovedEast, 'v', 0, 1)
	return gridAfterMovedSouth, anyMovedEast || anyMovedSouth
}

func answer(grid [][]rune) int {
	steps := 0
	for {
		nextGrid, anyMoved := step(grid)
		steps = steps + 1
		grid = nextGrid
		if !anyMoved {
			return steps
		}
	}
}

func main() {
	input := readParseInput("../input/day25.txt")
	fmt.Println(answer(input))
}
