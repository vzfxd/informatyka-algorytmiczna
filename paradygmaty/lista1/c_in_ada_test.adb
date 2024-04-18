with Ada.Text_IO;
with c_in_ada; use c_in_ada;

procedure c_in_ada_test is
    f : UInt64;
    f2 : UInt64;
    g : UInt64;
    g2 : UInt64;
    c : Coefficients;
    c2 : Coefficients;
begin
    f := c_in_ada.Factorial_Recursive(3);
    f2 := c_in_ada.Factorial_Iterative(3);
    Ada.Text_IO.Put_Line("3! = " & UInt64'Image(f));
    Ada.Text_IO.Put_Line("3! = " & UInt64'Image(f2));

    g := c_in_ada.GCD_Recursive(54, 36);
    g2 := c_in_ada.GCD_Iterative(54, 36);
    Ada.Text_IO.Put_Line("gcd(54, 36) = " & UInt64'Image(g));
    Ada.Text_IO.Put_Line("gcd(54, 36) = " & UInt64'Image(g2));
    

    c := c_in_ada.Diophantine_Solution_Recursive(10, 6, 14);
    c2 := c_in_ada.Diophantine_Solution_Iterative(10, 6, 14);
    Ada.Text_IO.Put_Line("10x + 6y = 14 || " & Int64'Image(c.x) & "," & Int64'Image(c.y));
    Ada.Text_IO.Put_Line("10x + 6y = 14 || " & Int64'Image(c2.x) & "," & Int64'Image(c2.y));
end c_in_ada_test;