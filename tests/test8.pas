program test8;

var
    a, g, b : real;

function power(num1, num2: real) : real;
var
    i, result: real;
begin
    if(num2 <= 0) then
        result := 1
    else
        result := num1 * power(num1, num2 - 1);
    power := result; 
end;

begin
    g := 22;
    b := 6;
    var
        b, c : real;
    for a := 8 to g do
        begin
            b := power(a, 4);
            writeln(b);
        end; 
    writeln(b);
end.