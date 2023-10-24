package main

import (
	"fmt"
	"math/rand"
	"strings"
	"sync"
	"time"
)

const (
	deploy_prob    float64 = 0.10
	move_prob      float64 = 0.75
	traveler_delay int     = 6
	camera_delay   int     = 2
	m              int     = 5
	n              int     = 5
)

var (
	s = rand.NewSource(time.Now().Unix())
	r = rand.New(s)
)

func traveler_life(id int, mutex *sync.Mutex, grid [][]int) {
	x := -1
	y := -1
	for {
		time.Sleep(time.Duration((r.Intn(traveler_delay-camera_delay) + camera_delay) * int(time.Second)))
		p := r.Float64()

		if x == -1 && y == -1 && p < deploy_prob {
			new_y := r.Intn(m)
			new_x := r.Intn(n)

			mutex.Lock()

			if grid[new_y][new_x] == -1 {
				grid[new_y][new_x] = id

				x = new_x
				y = new_y
			}

			mutex.Unlock()
		} else if x != -1 && y != -1 && p < move_prob {
			moves := [][]int{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
			rand_idx := r.Intn(len(moves))
			rand_move := moves[rand_idx]

			mutex.Lock()

			new_y := y + rand_move[0]
			new_x := x + rand_move[1]

			if new_y < m && new_y > 0 && new_x < n && new_x > 0 && grid[new_y][new_x] == -1 {
				grid[new_y][new_x] = id
				grid[y][x] = -1
				x = new_x
				y = new_y
			}

			mutex.Unlock()
		}
	}
}

func camera(grid [][]int, mutex *sync.Mutex) {
	photo := make([][]int, m)
	for i := range grid {
		photo[i] = make([]int, n)
	}

	for {
		time.Sleep(time.Duration(camera_delay * int(time.Second)))

		mutex.Lock()

		for idx, wiersz := range grid {
			fmt.Print(" ")
			for i := 0; i < n; i++ {
				if idx != 0 && photo[idx][i] == grid[idx-1][i] && photo[idx-1][i] == grid[idx][i] && !(grid[idx][i] == -1 && grid[idx-1][i] == -1) {
					fmt.Print("   ")
					continue
				}
				fmt.Print("== ")
			}
			fmt.Println()
			for i, wartosc := range wiersz {
				if i != 0 && photo[idx][i] == grid[idx][i-1] && photo[idx][i-1] == grid[idx][i] && !(grid[idx][i] == -1 && grid[idx][i-1] == -1) {
					if wartosc == -1 {
						fmt.Printf("   ")
					} else {
						fmt.Printf(" %2d", wartosc)
					}
					continue
				}
				if wartosc == -1 {
					fmt.Printf("|  ")
				} else {
					fmt.Printf("|%2d", wartosc)
				}
			}
			fmt.Println("|")
		}
		fmt.Print(" ")
		fmt.Print(strings.Repeat("== ", n))
		fmt.Print("\n\n\n")

		save_photo(photo, grid)

		mutex.Unlock()
	}
}

func save_photo(photo [][]int, grid [][]int) {
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			photo[i][j] = grid[i][j]
		}
	}
}

func main() {
	const max_k = m*n - 1
	var mutex sync.Mutex

	grid := make([][]int, m)
	for i := range grid {
		grid[i] = make([]int, n)
	}

	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			grid[i][j] = -1
		}
	}

	for id := 0; id < max_k; id++ {
		go traveler_life(id, &mutex, grid)
	}

	go camera(grid, &mutex)

	var input string
	fmt.Scanln(&input)
}
