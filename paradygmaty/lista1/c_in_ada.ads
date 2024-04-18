with Interfaces.C; use Interfaces.C;

package c_in_ada is
    subtype Int64 is Interfaces.C.long;
    subtype UInt64 is Interfaces.C.unsigned_long;

    type Coefficients is record
        X : Int64;
        Y : Int64;
    end record;
    pragma Convention (C, Coefficients);

    function Factorial_Iterative(N : UInt64) return UInt64;
    pragma Import(C, Factorial_Iterative, "factorial_iterative");

    function Factorial_Recursive(N : UInt64) return UInt64;
    pragma Import(C, Factorial_Recursive, "factorial_recursive");

    function GCD_Iterative(A, B : UInt64) return UInt64;
    pragma Import(C, GCD_Iterative, "gcd_iterative");

    function GCD_Recursive(A, B : UInt64) return UInt64;
    pragma Import(C, GCD_Recursive, "gcd_recursive");

    function Diophantine_Solution_Iterative(A, B, C: Int64) return Coefficients;
    pragma Import(C, Diophantine_Solution_Iterative, "diophantine_solution_iterative");

    function Diophantine_Solution_Recursive(A, B, C: Int64) return Coefficients;
    pragma Import(C, Diophantine_Solution_Recursive, "diophantine_solution_recursive");
end c_in_ada;