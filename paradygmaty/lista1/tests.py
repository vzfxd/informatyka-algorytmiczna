from libs.biblio import *

def main():
    print(f"3! = {factorial_recursive(3)}")
    print(f"3! = {factorial_iterative(3)}")
    print(f"gcd(54,36) = {gcd_recursive(54,36)}")
    print(f"gcd(54,36) = {gcd_iterative(54,36)}")
    print(f"10x + 6y = 14 || {diophantine_solution_recursive(10,6,14)}")
    print(f"10x + 6y = 14 || {diophantine_solution_iterative(10,6,14)}")

if __name__ == "__main__":
    main()