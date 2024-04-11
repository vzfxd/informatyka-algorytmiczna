function Factorial_Iterative(N : Integer) return Integer is
    Result : Integer := 1;
begin
    if N < 2 then
        return N;
    else
        for I in 1 .. N loop
            Result := Result * I;
        end loop;
        return Result;
    end if;
end Factorial_Iterative;

function Factorial_Recursive(N : Integer) return Integer is
begin
    if N < 2 then
        return N;
    else
        return N * Factorial_Recursive(N - 1);
    end if;
end Factorial_Recursive;

function GCD_Iterative(A, B : Integer) return Integer is
begin
    while B /= 0 loop
        (A, B) := (B, A mod B);
    end loop;
    return A;
end GCD_Iterative;

function GCD_Recursive(A, B : Integer) return Integer is
begin
    if B = 0 then
        return A;
    else
        return GCD_Recursive(B, A mod B);
    end if;
end GCD_Recursive;

