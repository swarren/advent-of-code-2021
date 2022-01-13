package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type AxisRange struct {
	min, max int
}

type Cube struct {
	axes [3]AxisRange
}

type Command struct {
	on   bool
	cube Cube
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func parseOn(son string) bool {
	return son == "on"
}

func parseAxis(saxis string) AxisRange {
	sAllVals := strings.Split(saxis, "=")[1]
	sVals := strings.Split(sAllVals, "..")
	sMin := sVals[0]
	sMax := sVals[1]
	iMin, err := strconv.Atoi(sMin)
	check(err)
	iMax, err := strconv.Atoi(sMax)
	check(err)
	return AxisRange{iMin, iMax}
}

func readParseInput(fn string) []Command {
	f, err := os.Open(fn)
	check(err)
	defer f.Close()

	commands := []Command{}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		words := strings.Split(line, " ")
		son := words[0]
		scube := words[1]
		on := parseOn(son)
		saxes := strings.Split(scube, ",")
		axes := [3]AxisRange{}
		for axis := 0; axis < 3; axis++ {
			axes[axis] = parseAxis(saxes[axis])
		}
		command := Command{on, Cube{axes}}
		commands = append(commands, command)
	}

	return commands
}

func discardIgnoredCommands(commands []Command) []Command {
	outOfRange := func(ar *AxisRange) bool {
		return ar.min < -50 || ar.max > 50
	}

	commandsOut := []Command{}
	for _, command := range commands {
		inRange := true
		for axis := 0; axis < 3; axis++ {
			if outOfRange(&command.cube.axes[axis]) {
				inRange = false
				break
			}
		}
		if inRange {
			commandsOut = append(commandsOut, command)
		}
	}
	return commandsOut
}

type HitCube struct {
	hit  bool
	cube *Cube
}

func imin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func imax(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func genSplitCubesByAxis(cube, by *Cube, axis int) []*HitCube {
	cubeAr := cube.axes[axis]
	byAr := by.axes[axis]
	if byAr.max < cubeAr.min {
		return []*HitCube{&HitCube{false, cube}}
	}
	if byAr.min > cubeAr.max {
		return []*HitCube{&HitCube{false, cube}}
	}
	if byAr.min <= cubeAr.min && byAr.max >= cubeAr.max {
		return []*HitCube{&HitCube{true, cube}}
	}

	splitHitCubes := []*HitCube{}
	if byAr.min > cubeAr.min {
		newCube := *cube
		newCube.axes[axis].max = byAr.min - 1
		newHitCube := HitCube{false, &newCube}
		splitHitCubes = append(splitHitCubes, &newHitCube)
	}

	newCube := *cube
	newCube.axes[axis].min = imax(byAr.min, cubeAr.min)
	newCube.axes[axis].max = imin(byAr.max, cubeAr.max)
	newHitCube := HitCube{true, &newCube}
	splitHitCubes = append(splitHitCubes, &newHitCube)

	if byAr.max < cubeAr.max {
		newCube := *cube
		newCube.axes[axis].min = byAr.max + 1
		newHitCube := HitCube{false, &newCube}
		splitHitCubes = append(splitHitCubes, &newHitCube)
	}

	return splitHitCubes
}

func genSplitCubes(cube, by *Cube) []*Cube {
	toSplitCubes := []*Cube{cube}
	splitCubes := []*Cube{}
	for axis := 0; axis < 3; axis++ {
		newToSplitCubes := []*Cube{}
		for _, cubeToSplit := range toSplitCubes {
			hitCubes := genSplitCubesByAxis(cubeToSplit, by, axis)
			for _, hitCube := range hitCubes {
				if hitCube.hit {
					newToSplitCubes = append(newToSplitCubes, hitCube.cube)
				} else {
					splitCubes = append(splitCubes, hitCube.cube)
				}
			}
		}
		toSplitCubes = newToSplitCubes
	}

	return splitCubes
}

func cubePoints(cube *Cube) int {
	axisSize := func(ar *AxisRange) int {
		return ar.max - ar.min + 1
	}

	points := 1
	for axis := 0; axis < 3; axis++ {
		points *= axisSize(&cube.axes[axis])
	}
	return points
}

func applyCommands(commands []Command) int {
	onCubes := []*Cube{}

	for _, command := range commands {
		newOnCubes := []*Cube{}
		for _, onCube := range onCubes {
			splitCubes := genSplitCubes(onCube, &command.cube)
			for _, splitCube := range splitCubes {
				newOnCubes = append(newOnCubes, splitCube)
			}
		}
		if command.on {
			newOnCube := command.cube
			newOnCubes = append(newOnCubes, &newOnCube)
		}
		onCubes = newOnCubes
	}

	count := 0
	for _, onCube := range onCubes {
		count += cubePoints(onCube)
	}
	return count
}

func main() {
	input := readParseInput("../input/day22.txt")
	input = discardIgnoredCommands(input)
	result := applyCommands(input)
	fmt.Println(result)
}
