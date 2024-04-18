with Interfaces.C; use Interfaces.C;

package biblio is

    subtype Int64 is Interfaces.C.long;
    subtype UInt64 is Interfaces.C.unsigned_long;

    type Coefficients is record
        X : Int64;
        Y : Int64;
    end record;
    pragma Convention(C, Coefficients);

    function Factorial_Iterative(N : UInt64) return UInt64;
    pragma Export(C, Factorial_Iterative, "ada_factorial_iterative");

    function Factorial_Recursive(N : UInt64) return UInt64;
    pragma Export(C, Factorial_Recursive, "ada_factorial_recursive");

    function GCD_Iterative(A, B : UInt64) return UInt64;
    pragma Export(C, GCD_Iterative, "ada_gcd_iterative");

    function GCD_Recursive(A, B : UInt64) return UInt64;
    pragma Export(C, GCD_Recursive, "ada_gcd_recursive");

    function Diophantine_Solution_Iterative(A, B, C: Int64) return Coefficients;
    pragma Export(C, Diophantine_Solution_Iterative, "ada_diophantine_solution_iterative");

    function Diophantine_Solution_Recursive(A, B, C: Int64) return Coefficients;
    pragma Export(C, Diophantine_Solution_Recursive, "ada_diophantine_solution_recursive");

end biblio;