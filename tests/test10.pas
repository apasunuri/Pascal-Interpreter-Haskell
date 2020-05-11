program test10;

var
    x, y, z: real;

procedure sub2();
var
    x, y: real;
begin
    x := 2;
    y := x + z;
    writeln(y);
end;

procedure sub1();
var
    x, z: real;
begin
    x := 8;
    z := 9;
    sub2();
    x := y * z + x;
    writeln(x);
end;

begin
    x := 1;
    y := 3;
    z := 5;
    sub1();
    sub2();
end.