package main

import (
	"sync"
)

type Response struct {
	allowed bool
}

type Semaphore struct {
	mutex    sync.Mutex
	queue    []chan Response
	resource int
}

func NewSemaphore(initial int) *Semaphore {
	return &Semaphore{
		resource: initial,
		queue:    make([]chan Response, 0),
	}
}

func (s *Semaphore) Wait(res chan Response, id int) {
	s.mutex.Lock()

	if s.resource > 0 {
		s.resource--
		res <- Response{allowed: true}
	} else {
		res <- Response{allowed: false}
		s.queue = append(s.queue, res)
	}

	s.mutex.Unlock()
}

func (s *Semaphore) Signal() {
	s.mutex.Lock()

	if len(s.queue) > 0 {
		front := s.queue[0]
		front <- Response{allowed: true}
		s.queue = s.queue[1:]
	} else {
		s.resource++
	}

	s.mutex.Unlock()
}
