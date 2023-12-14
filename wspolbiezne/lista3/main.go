package main

import (
	"os"
	"strconv"
)

func main() {
	zad := os.Args[1]

	if zad == "zad1_sem" {
		n, _ := strconv.Atoi(os.Args[2])
		zad1_sem(n)
	} else if zad == "zad1_mon" {
		n, _ := strconv.Atoi(os.Args[2])
		zad1_mon(n)
	} else if zad == "zad2" {
		m, _ := strconv.Atoi(os.Args[2])
		n, _ := strconv.Atoi(os.Args[3])
		zad2(m, n)
	}
}
