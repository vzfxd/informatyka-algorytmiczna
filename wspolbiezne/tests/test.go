package main

import (
	"fmt"
)

func main() {
	var arr1 = [3]int{1, 2, 3}
	var arr2 = [3]int{4, 5, 6}

	for i := range arr1 {
		arr1[i] = arr2[i]
		arr2[i] = arr2[i] * 10
	}

	for i := range arr1 {
		fmt.Printf("%d\t", arr1[i])
	}
	fmt.Println()
	for i := range arr2 {
		fmt.Printf("%d\t", arr2[i])
	}

}
