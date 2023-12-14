package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"sync"
	"time"
)

var table []int
var table_mutex sync.Mutex

func think() {
	t := rand.Intn(5) + 5
	time.Sleep(time.Duration(t) * time.Second)
}

func eat() {
	think()
}

func filozof(i int, forks []*Semaphore, n int) {
	res := make(chan Response)
	order := [2]int{i, (i + 1) % n}
	if i == n-1 {
		order[0], order[1] = order[1], order[0]
	}

	for {
		go forks[order[0]].Wait(res, i)
		f1 := <-res
		if f1.allowed == false {
			f1 = <-res
		}

		go forks[order[1]].Wait(res, i)
		f2 := <-res
		if f2.allowed == false {
			f2 = <-res
		}

		table_mutex.Lock()
		table = append(table, order[0], i, order[1])
		print_table(table)
		table_mutex.Unlock()

		eat()

		var wg sync.WaitGroup
		wg.Add(1)

		go func() {
			defer wg.Done()
			forks[order[0]].Signal()
			forks[order[1]].Signal()
		}()

		wg.Wait()

		table_mutex.Lock()
		for j := 1; j < len(table); j += 3 {
			if table[j] == i {
				table = append(table[:j-1], table[j+2:]...)
			}
		}
		print_table(table)
		table_mutex.Unlock()

		think()
	}
}

func print_table(table []int) {
	s := ""
	for i := 0; i < len(table); i++ {
		s += strconv.Itoa(table[i])
		if (i+1)%3 == 0 {
			fmt.Print(s + "\n")
			s = ""
		}
	}
	fmt.Print("========================\n")
}

func zad1_sem(n int) {
	table = make([]int, 0)

	forks := make([]*Semaphore, n)
	for i := 0; i < n; i++ {
		forks[i] = NewSemaphore(1)
	}

	for i := 0; i < n; i++ {
		go filozof(i, forks, n)
	}

	var input string
	fmt.Scanln(&input)
}
