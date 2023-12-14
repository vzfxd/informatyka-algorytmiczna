package main

import (
	"sync"
)

type Monitor struct {
	mutex sync.Mutex
	cond  []*sync.Cond
	forks []int
}

func NewMonitor(n int) *Monitor {
	monitor := &Monitor{}
	monitor.cond = make([]*sync.Cond, n)
	monitor.forks = make([]int, n)
	for i := 0; i < n; i++ {
		monitor.cond[i] = sync.NewCond(&monitor.mutex)
		monitor.forks[i] = 2
	}
	return monitor
}

func (m *Monitor) TakeFork(i int, n int) {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	if m.forks[i] != 2 {
		m.cond[i].Wait()
	}

	m.forks[(i+1)%n]--
	m.forks[(i+4)%n]--
}

func (m *Monitor) ReleaseFork(i int, n int) {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	m.forks[(i+1)%n]++
	m.forks[(i+4)%n]++

	if m.forks[(i+1)%n] == 2 {
		m.cond[(i+1)%n].Signal()
	}

	if m.forks[(i+4)%n] == 2 {
		m.cond[(i+4)%n].Signal()
	}
}

func philosopher(id int, monitor *Monitor, wg *sync.WaitGroup, n int) {
	defer wg.Done()

	for {
		monitor.TakeFork(id, n)

		table_mutex.Lock()
		table = append(table, id, id, (id+1)%n)
		print_table(table)
		table_mutex.Unlock()

		eat()
		monitor.ReleaseFork(id, n)

		table_mutex.Lock()
		for j := 1; j < len(table); j += 3 {
			if table[j] == id {
				table = append(table[:j-1], table[j+2:]...)
			}
		}
		print_table(table)
		table_mutex.Unlock()

		think()
	}
}

func zad1_mon(n int) {
	table = make([]int, 0)
	monitor := NewMonitor(n)
	var wg sync.WaitGroup

	for i := 0; i < n; i++ {
		wg.Add(1)
		go philosopher(i, monitor, &wg, n)
	}

	wg.Wait()
}
