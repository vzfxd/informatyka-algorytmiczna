package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"strings"
	"time"
)

type Request struct {
	traveler   string
	leave      bool
	move_in    bool
	rep_chan   chan Response
	req_chan   chan Request
	traveler_y int
	traveler_x int
}

type Response struct {
	allowed bool
	kill    bool
}

type Node struct {
	traveler        string
	traveler_origin int
	requests        chan Request
	responses       chan Response
}

const (
	deploy_prob    float64 = 0.05
	move_prob      float64 = 0.90
	traveler_delay int     = 6
	camera_delay   int     = 2
	m              int     = 5
	n              int     = 5
	d              int     = 10
)

var (
	s = rand.NewSource(time.Now().Unix())
	r = rand.New(s)
)

func server(node *Node, y int, x int, grid [][]Node) {
	var wild_req_chan chan Request
	timeout := time.After(time.Duration(d/2) * time.Second)
	for {
		select {
		case request := <-(*node).requests:
			if request.leave {
				(*node).traveler = "-1"
			}

			if request.move_in {
				allowed := false
				kill := false

				if (*node).traveler == "-1" {
					allowed = true
				} else if (*node).traveler == "*" {
					wild_req_chan <- Request{leave: true, rep_chan: (*node).responses}
					response := <-(*node).responses
					if response.allowed {
						allowed = true
					}
				} else if (*node).traveler == "#" {
					kill = true
					(*node).traveler = "-1"
				}

				if allowed {
					wild_req_chan = request.req_chan
					(*node).traveler = request.traveler
				}

				if allowed || kill {
					t_x := request.traveler_x
					t_y := request.traveler_y
					if t_x != -1 && t_y != -1 {
						var t_o int
						if t_x < x {
							t_o = 1
						}
						if t_y < y {
							t_o = 2
						}
						if t_x > x {
							t_o = 3
						}
						if t_y > y {
							t_o = 4
						}
						(*node).traveler_origin = t_o
					}
				}
				request.rep_chan <- Response{allowed: allowed, kill: kill}
			}
		case <-timeout:
			if (*node).traveler == "-1" {
				p := r.Float64()
				if p < deploy_prob {
					if p < deploy_prob/2 {
						req_chan := make(chan Request)
						go wild_traveler(grid, y, x, time.Now(), req_chan)
						wild_req_chan = req_chan
						(*node).traveler = "*"
					} else {
						(*node).traveler = "#"
					}
				}
			}
			timeout = time.After(time.Duration(d/2) * time.Second)
		}
	}
}

func wild_traveler(grid [][]Node, y int, x int, spawn time.Time, req_chan chan Request) {
	var rep_chan = make(chan Response)
	timeout := time.After(time.Duration(d*2) * time.Second)
	for {
		select {
		case <-timeout:
			t := time.Now()
			elapsed := t.Sub(spawn) / 1e6
			lifespan := elapsed.Nanoseconds() / 1000
			if lifespan >= 10 {
				node := grid[y][x]
				node.requests <- Request{
					leave: true,
				}
				return
			}
		case request := <-req_chan:
			if request.leave {
				can_leave := false
				kys := false
				moves := [][]int{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}

				for i := range moves {
					new_y := moves[i][0] + y
					new_x := moves[i][1] + x

					if new_y < m && new_y > 0 && new_x < n && new_x > 0 {
						node := grid[new_y][new_x]
						node.requests <- Request{
							traveler:   "*",
							leave:      false,
							move_in:    true,
							rep_chan:   rep_chan,
							traveler_y: y,
							traveler_x: x,
						}

						response := <-rep_chan
						kys = response.kill
						if response.allowed || response.kill {
							can_leave = true
							x = new_x
							y = new_y
							break
						}
					}
				}
				request.rep_chan <- Response{allowed: can_leave}
				if kys {
					fmt.Print("dziki sie zabija\n")
					return
				}
			}
		}
	}
}

func traveler_life(id string, grid [][]Node, traces [][]int) {
	x := -1
	y := -1
	var rep_chan = make(chan Response)
	for {
		time.Sleep(time.Duration((r.Intn(traveler_delay-camera_delay) + camera_delay) * int(time.Second)))
		p := r.Float64()

		if x == -1 && y == -1 && p < deploy_prob {
			new_y := r.Intn(m)
			new_x := r.Intn(n)

			node := grid[new_y][new_x]
			node.requests <- Request{
				traveler:   id,
				leave:      false,
				move_in:    true,
				rep_chan:   rep_chan,
				traveler_y: y,
				traveler_x: x,
			}

			response := <-rep_chan
			if response.allowed {
				x = new_x
				y = new_y
			}
		}

		if x != -1 && y != -1 && p < move_prob {
			moves := [][]int{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
			rand_idx := r.Intn(len(moves))
			rand_move := moves[rand_idx]

			new_y := y + rand_move[0]
			new_x := x + rand_move[1]

			if new_y < m && new_y > 0 && new_x < n && new_x > 0 {
				node := grid[new_y][new_x]
				node.requests <- Request{
					traveler:   id,
					leave:      false,
					move_in:    true,
					rep_chan:   rep_chan,
					traveler_y: y,
					traveler_x: x,
				}

				response := <-rep_chan
				if response.allowed {
					prev_node := grid[y][x]
					prev_node.requests <- Request{
						leave: true,
					}
					x = new_x
					y = new_y
				}

				if response.kill {
					prev_node := grid[y][x]
					prev_node.requests <- Request{
						leave: true,
					}
					fmt.Printf("%s sie zabija\n", id)
					return
				}
			}
		}
	}
}

func camera(grid [][]Node, traces [][]int) {
	photo := make([][]Node, m)
	for i := range grid {
		photo[i] = make([]Node, n)
	}

	for {
		time.Sleep(time.Duration(camera_delay * int(time.Second)))

		save_photo(photo, grid)

		for y := range photo {
			fmt.Print(" ")
			for i := 0; i < n; i++ {
				if y > 0 {
					node := photo[y][i]
					node_above := photo[y-1][i]
					if node.traveler_origin == 2 || node_above.traveler_origin == 4 {
						fmt.Print("   ")
					} else {
						fmt.Print("== ")
					}
				} else {
					fmt.Print("== ")
				}
			}
			fmt.Println()
			for x := range photo[y] {
				node := photo[y][x]
				if x > 0 {
					left_node := photo[y][x-1]
					if node.traveler_origin == 1 || left_node.traveler_origin == 3 {
						if node.traveler == "-1" {
							fmt.Print("   ")
						} else {
							fmt.Printf(" %2s", node.traveler)
						}
					} else {
						if node.traveler == "-1" {
							fmt.Printf("|  ")
						} else {
							fmt.Printf("|%2s", node.traveler)
						}
					}

				} else {
					if node.traveler == "-1" {
						fmt.Printf("|  ")
					} else {
						fmt.Printf("|%2s", node.traveler)
					}
				}
			}
			fmt.Println("|")
		}
		fmt.Print(" ")
		fmt.Print(strings.Repeat("== ", n))
		fmt.Print("\n\n\n")
	}

}

func save_photo(photo [][]Node, grid [][]Node) {
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			photo[i][j] = grid[i][j]
			grid[i][j].traveler_origin = -1
		}
	}
}

func main() {
	const max_k = m*n - 1

	grid := make([][]Node, m)
	traces := make([][]int, m)
	for i := range grid {
		grid[i] = make([]Node, n)
		traces[i] = make([]int, n)
	}

	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			node := Node{
				traveler:        "-1",
				traveler_origin: -1,
				requests:        make(chan Request),
				responses:       make(chan Response),
			}
			grid[i][j] = node
			go server(&grid[i][j], i, j, grid)
		}
	}

	for id := 0; id < max_k; id++ {
		go traveler_life(strconv.Itoa(id), grid, traces)
	}

	go camera(grid, traces)

	var input string
	fmt.Scanln(&input)
}
