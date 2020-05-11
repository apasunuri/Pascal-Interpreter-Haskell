program test4;

var
    a, b: real;

begin 
    a := 10;
    while a < 20 do
        begin
            writeln('value of a:');
            writeln(a);
            a := a + 1;
        end;
    writeln();
    for b := 1 to 8 do
        begin
            writeln('SIN, COS, EXP, LN of b:');
            writeln(sin(b));
            writeln(cos(b));
            writeln(exp(b));
            writeln(ln(b));
            writeln();
        end; 
end.