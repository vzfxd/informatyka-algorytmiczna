from ctypes import *
import os,sys

so = os.path.dirname(os.path.abspath(sys.argv[0])) + "/c.so"

C_functions = CDLL(so)

class Coefficients(Structure):
    _fields_ = [("x", c_long),
                ("y", c_long)]

def factorial_recursive(n : c_ulong) -> c_ulong:
    return C_functions.factorial_recursive(n)

def factorial_iterative(n : c_ulong) -> c_ulong:
    return C_functions.factorial_iterative(n)

def gcd_iterative(a: c_ulong, b: c_ulong) -> c_ulong:
    return C_functions.gcd_iterative(a, b)

def gcd_recursive(a: c_ulong, b: c_ulong) -> c_ulong:
    return C_functions.gcd_recursive(a, b)

def diophantine_solution_recursive(a: c_long, b: c_long, c: c_long) -> Coefficients:
    func = C_functions.diophantine_solution_recursive
    func.restype = Coefficients
    return func(a, b, c)

def diophantine_solution_iterative(a: c_long, b: c_long, c: c_long) -> Coefficients:
    func = C_functions.diophantine_solution_iterative
    func.restype = Coefficients
    return func(a, b, c)

c = diophantine_solution_recursive(10,6,14)
c2 = diophantine_solution_iterative(10,6,14)
print(f"3! = {factorial_recursive(3)}")
print(f"3! = {factorial_iterative(3)}")
print(f"gcd(54,36) = {gcd_recursive(54,36)}")
print(f"gcd(54,36) = {gcd_iterative(54,36)}")
print(f"10x + 6y = 14 || {c.x,c.y}")
print(f"10x + 6y = 14 || {c2.x,c2.y}")