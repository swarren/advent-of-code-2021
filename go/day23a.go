package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type Coord struct {
	x, y int
}

type Move struct {
	destination Coord
	cost        int
}

type Amphipod struct {
	abcd     byte
	hasMoved bool
}

type Diagram map[Coord]Amphipod

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func parseSideRooms(diagram Diagram, line string, y int) {
	for x := 3; x <= 9; x += 2 {
		abcd := line[x]
		coord := Coord{x, y}
		amphipod := Amphipod{abcd, false}
		diagram[coord] = amphipod
	}
}

func readParseInput(fn string) Diagram {
	f, err := os.Open(fn)
	check(err)
	defer f.Close()
	r := bufio.NewReader(f)

	// top wall
	line, err := r.ReadString(10)
	check(err)
	// hallway
	line, err = r.ReadString(10)
	check(err)

	diagram := Diagram{}
	// top row side rooms
	line, err = r.ReadString(10)
	check(err)
	parseSideRooms(diagram, line, 2)
	// bottom row side rooms
	line, err = r.ReadString(10)
	check(err)
	parseSideRooms(diagram, line, 3)

	return diagram
}

func amphipodMoveCost(amphipod Amphipod) int {
	switch amphipod.abcd {
	case 'A':
		return 1
	case 'B':
		return 10
	case 'C':
		return 100
	case 'D':
		return 1000
	default:
		panic(fmt.Errorf("Invalid abcd %c", amphipod.abcd))
	}
}

func amphipodHomeX(amphipod Amphipod) int {
	return int(amphipod.abcd-'A')*2 + 3
}

func amphipodMovesMoved(diagram Diagram, startCoord Coord, amphipod Amphipod) []Move {
	homeX := amphipodHomeX(amphipod)

	// Home room full
	homeCoordTop := Coord{homeX, 2}
	_, topFull := diagram[homeCoordTop]
	if topFull {
		return []Move{}
	}

	// Home room occupied by a different type
	homeCoordBot := Coord{homeX, 3}
	amphipodInRoomBot, botFull := diagram[homeCoordBot]
	if botFull && amphipodInRoomBot.abcd != amphipod.abcd {
		return []Move{}
	}

	// Move through hallway
	moveCost := amphipodMoveCost(amphipod)
	xDir := 1
	if startCoord.x > homeX {
		xDir = -1
	}
	cost := 0
	coord := startCoord
	for coord.x != homeX {
		coord.x += xDir
		cost += moveCost
		// Something in hallway blocks us?
		_, blockedHallway := diagram[coord]
		if blockedHallway {
			return []Move{}
		}
	}

	if !botFull {
		return []Move{Move{homeCoordBot, cost + (2 * moveCost)}}
	} else {
		return []Move{Move{homeCoordTop, cost + (1 * moveCost)}}
	}
}

func amphipodMovesNotMoved(diagram Diagram, startCoord Coord, amphipod Amphipod) []Move {
	moveCost := amphipodMoveCost(amphipod)
	moves := []Move{}

	// Move to hallway
	cost := 0
	coord := startCoord
	for coord.y != 1 {
		coord.y -= 1
		cost += moveCost
		// Something in room blocks us?
		_, blockedRoom := diagram[coord]
		if blockedRoom {
			return []Move{}
		}
	}

	for xDir := -1; xDir <= 1; xDir += 2 {
		savedCost := cost
		savedCoord := coord

		for coord.x >= 2 && coord.x <= 10 {
			coord.x += xDir
			cost += moveCost
			// Something in hallway blocks us?
			_, blockedHallway := diagram[coord]
			if blockedHallway {
				break
			}

			// Can't stop outside a room
			outsideRoom := coord.x >= 3 && coord.x <= 9 && coord.x&1 != 0
			if !outsideRoom {
				moves = append(moves, Move{coord, cost})
			}
		}

		cost = savedCost
		coord = savedCoord
	}

	return moves
}

func amphipodMoves(diagram Diagram, startCoord Coord, amphipod Amphipod) []Move {
	if amphipod.hasMoved {
		return amphipodMovesMoved(diagram, startCoord, amphipod)
	} else {
		return amphipodMovesNotMoved(diagram, startCoord, amphipod)
	}
}

func diagramCopied(diagram Diagram) Diagram {
	copy := map[Coord]Amphipod{}
	for k, v := range diagram {
		copy[k] = v
	}
	return copy
}

func diagramAfterMove(diagram Diagram, from Coord, to Coord, amphipod Amphipod) Diagram {
	diagramNext := diagramCopied(diagram)
	delete(diagramNext, from)
	amphipodMoved := amphipod
	amphipodMoved.hasMoved = true
	diagramNext[to] = amphipodMoved
	return diagramNext
}

type SolverQueueItem struct {
	cost    int
	diagram Diagram
	index   int
}

type SolverQueue []*SolverQueueItem

func (sq SolverQueue) Len() int { return len(sq) }

func (sq SolverQueue) Less(i, j int) bool {
	return sq[i].cost < sq[j].cost
}

func (sq SolverQueue) Swap(i, j int) {
	sq[i], sq[j] = sq[j], sq[i]
	sq[i].index = i
	sq[j].index = j
}

func (sq *SolverQueue) Push(x interface{}) {
	n := len(*sq)
	sqi := x.(*SolverQueueItem)
	sqi.index = n
	*sq = append(*sq, sqi)
}

func (sq *SolverQueue) Pop() interface{} {
	old := *sq
	n := len(old)
	sqi := old[n-1]
	old[n-1] = nil
	sqi.index = -1
	*sq = old[0 : n-1]
	return sqi
}

func isTargetState(diagram Diagram) bool {
	for x := 3; x <= 9; x += 2 {
		wantAbcd := 'A' + byte((x-3)/2)
		for y := 2; y <= 3; y++ {
			coord := Coord{x, y}
			amphipod, present := diagram[coord]
			if !present {
				return false
			}
			if amphipod.abcd != wantAbcd {
				return false
			}
		}
	}
	return true
}

func diagStr(diagram Diagram) string {
	s := ""
	for y := 1; y <= 3; y++ {
		coord := Coord{0, y}
		for x := 1; x <= 11; x++ {
			coord.x = x
			amphipod, present := diagram[coord]
			if present {
				s = s + string(rune(amphipod.abcd))
			} else {
				s = s + "."
			}
		}
		s = s + " "
	}
	return s
}

func solve(initial Diagram) int {
	sq := make(SolverQueue, 0)
	heap.Push(&sq, &SolverQueueItem{cost: 0, diagram: initial})
	diagramCost := map[string]int{}
	diagramCost[diagStr(initial)] = 0

	for {
		sqi := heap.Pop(&sq).(*SolverQueueItem)
		if isTargetState(sqi.diagram) {
			return sqi.cost
		}
		for coord, amphipod := range sqi.diagram {
			for _, move := range amphipodMoves(sqi.diagram, coord, amphipod) {
				costNext := sqi.cost + move.cost
				diagramNext := diagramAfterMove(sqi.diagram, coord, move.destination, amphipod)
				s := diagStr(diagramNext)
				prevCost, present := diagramCost[s]
				if !present || costNext < prevCost {
					heap.Push(&sq, &SolverQueueItem{cost: costNext, diagram: diagramNext})
					diagramCost[s] = costNext
				}
			}
		}
	}
}

func main() {
	input := readParseInput("../input/day23.txt")
	result := solve(input)
	fmt.Println(result)
}
