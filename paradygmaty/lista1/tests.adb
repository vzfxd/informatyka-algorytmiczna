with Ada.Text_IO;
with biblio; use biblio;

procedure Tests is
    f : UInt64;
    f2 : UInt64;
    g : UInt64;
    g2 : UInt64;
    c : Coefficients;
    c2 : Coefficients;
begin
    f := biblio.Factorial_Recursive(3);
    f2 := biblio.Factorial_Iterative(3);
    Ada.Text_IO.Put_Line("3! = " & UInt64'Image(f));
    Ada.Text_IO.Put_Line("3! = " & UInt64'Image(f2));

    g := biblio.GCD_Recursive(54, 36);
    g2 := biblio.GCD_Iterative(54, 36);
    Ada.Text_IO.Put_Line("gcd(54, 36) = " & UInt64'Image(g));
    Ada.Text_IO.Put_Line("gcd(54, 36) = " & UInt64'Image(g2));
    

    c := biblio.Diophantine_Solution_Recursive(10, 6, 14);
    c2 := biblio.Diophantine_Solution_Iterative(10, 6, 14);
    Ada.Text_IO.Put_Line("10x + 6y = 14 || " & Int64'Image(c.x) & "," & Int64'Image(c.y));
    Ada.Text_IO.Put_Line("10x + 6y = 14 || " & Int64'Image(c2.x) & "," & Int64'Image(c2.y));

end Tests;