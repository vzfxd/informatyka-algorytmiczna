package body Biblio is

   function Factorial_Iterative(N : UInt64) return UInt64 is
      Result : UInt64 := 1;
   begin
      for I in 1 .. N loop
         Result := Result * I;
      end loop;
      return Result;
   end Factorial_Iterative;

   function Factorial_Recursive(N : UInt64) return UInt64 is
   begin
      if N < 2 then
         return N;
      else
         return N * Factorial_Recursive(N - 1);
      end if;
   end Factorial_Recursive;

   function GCD_Iterative(A, B : UInt64) return UInt64 is
        Temp : UInt64;
        A2 : UInt64;
        B2 : UInt64;
    begin
        A2 := A;
        B2 := B;
        while B2 /= 0 loop
            Temp := B2;
            B2 := A2 mod B2;
            A2 := Temp;
        end loop;
        return A2;
    end GCD_Iterative;

   function GCD_Recursive(A, B : UInt64) return UInt64 is
   begin
      if B = 0 then
         return A;
      else
         return GCD_Recursive(B, A mod B);
      end if;
   end GCD_Recursive;

   function Extended_GCD_Recursive(A, B : Int64; X, Y : out Int64) return Int64 is
   begin
      if B = 0 then
         X := 1;
         Y := 0;
         return A;
      else
         declare
            X1, Y1 : Int64;
         begin
            declare
               D : Int64 := Extended_GCD_Recursive(B, A mod B, X1, Y1);
            begin
               X := Y1;
               Y := X1 - (A / B) * Y1;
               return D;
            end;
         end;
      end if;
   end Extended_GCD_Recursive;

   function Extended_GCD_Iterative(A, B : Int64; X, Y : out Int64) return Int64 is
      X0, Y0, X1, Y1, Temp, Q : Int64;
      A2, B2 : Int64;
   begin
      X0 := 1;
      Y0 := 0;
      X1 := 0;
      Y1 := 1;
      A2 := A;
      B2 := B;
      while B2 /= 0 loop
         Q := A2 / B2;
         Temp := B2;
         B2 := A2 mod B2;
         A2 := Temp;
         Temp := X1;
         X1 := X0 - Q * X1;
         X0 := Temp;
         Temp := Y1;
         Y1 := Y0 - Q * Y1;
         Y0 := Temp;
      end loop;
      X := X0;
      Y := Y0;
      return A2;
   end Extended_GCD_Iterative;

   function Diophantine_Solution_Recursive(A, B, C : Int64) return Coefficients is
      Coeffs : Coefficients;
      X, Y : Int64;
      D : Int64 := Extended_GCD_Recursive(A, B, X, Y);
   begin
      if C mod D = 0 then
         declare
            Factor : Int64 := C / D;
         begin
            X := X * Factor;
            Y := Y * Factor;
            Coeffs.X := X;
            Coeffs.Y := Y;
         end;
      end if;
      return Coeffs;
   end Diophantine_Solution_Recursive;

   function Diophantine_Solution_Iterative(A, B, C : Int64) return Coefficients is
      Coeffs : Coefficients;
      X, Y : Int64;
      D : Int64 := Extended_GCD_Iterative(A, B, X, Y);
   begin
      if C mod D = 0 then
         declare
            Factor : Int64 := C / D;
         begin
            X := X * Factor;
            Y := Y * Factor;
            Coeffs.X := X;
            Coeffs.Y := Y;
         end;
      end if;
      return Coeffs;
   end Diophantine_Solution_Iterative;

end Biblio;
