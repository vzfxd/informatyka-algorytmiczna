package main

import (
	"fmt"
	"strconv"
	"sync"
)

type MonitorRW struct {
	mutex           sync.Mutex
	OK_to_read      *sync.Cond
	OK_to_write     *sync.Cond
	readers         int
	writing         bool
	writers_waiting int
	readers_waiting int
}

func NewMonitorRW() *MonitorRW {
	monitor := &MonitorRW{
		readers:         0,
		writers_waiting: 0,
		readers_waiting: 0,
		writing:         false,
	}

	monitor.OK_to_read = sync.NewCond(&monitor.mutex)
	monitor.OK_to_write = sync.NewCond(&monitor.mutex)

	return monitor
}

func print_attendance(table []int) {
	s := ""
	for i := 0; i < len(table); i++ {
		s += strconv.Itoa(table[i])
	}
	fmt.Print(s + "\n")
	fmt.Print("========================\n")
}

func reader(id int, m *MonitorRW, wg *sync.WaitGroup) {
	defer wg.Done()

	for {
		sleep()
		m.start_read()

		table_mutex.Lock()
		fmt.Printf("reader %d czyta \n", id)
		table = append(table, id)
		print_attendance(table)
		table_mutex.Unlock()

		sleep()
		m.stop_read()

		table_mutex.Lock()
		fmt.Printf("reader %d wychodzi \n", id)
		for j := 0; j < len(table); j++ {
			if table[j] == id {
				table = append(table[:j], table[j+1:]...)
			}
		}
		print_attendance(table)
		table_mutex.Unlock()
	}
}

func (m *MonitorRW) start_read() {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	if m.writing || m.writers_waiting > 0 {
		m.readers_waiting++
		m.OK_to_read.Wait()
		m.readers_waiting--
	}

	m.readers++
	m.OK_to_read.Broadcast()
}

func (m *MonitorRW) stop_read() {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	m.readers--
	if m.readers == 0 {
		m.OK_to_write.Signal()
	}
}

func writer(id int, m *MonitorRW, wg *sync.WaitGroup) {
	defer wg.Done()

	for {
		sleep()
		m.start_write()

		table_mutex.Lock()
		fmt.Printf("writer %d pisze \n", id)
		table = append(table, id)
		print_attendance(table)
		table_mutex.Unlock()

		sleep()
		m.stop_write()

		table_mutex.Lock()
		fmt.Printf("writer %d wychodzi \n", id)
		for j := 0; j < len(table); j++ {
			if table[j] == id {
				table = append(table[:j], table[j+1:]...)
			}
		}
		print_attendance(table)
		table_mutex.Unlock()
	}
}

func (m *MonitorRW) start_write() {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	if m.readers != 0 || m.writing {
		m.writers_waiting++
		m.OK_to_write.Wait()
		m.writers_waiting--
	}

	m.writing = true
}

func (m *MonitorRW) stop_write() {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	m.writing = false

	if m.readers_waiting > 0 {
		m.OK_to_read.Broadcast()
	} else {
		m.OK_to_write.Signal()
	}
}

func sleep() {
	think()
}

func zad2(m int, n int) {
	table = make([]int, 0)
	monitor := NewMonitorRW()
	var wg sync.WaitGroup

	for i := 0; i < m; i++ {
		wg.Add(1)
		go reader(i, monitor, &wg)
	}

	for i := 0; i < n; i++ {
		wg.Add(1)
		go writer(i, monitor, &wg)
	}

	wg.Wait()
}
