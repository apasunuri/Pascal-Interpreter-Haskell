program test6;

var 
    a, b : real;

begin
    a := 0;
    b := 6;
    var
        b : real;
    while (a <= 9) do
    begin
        b := ln(a);
        case(sqrt(a)) of
            1 : begin
                    writeln('a is a perfect square');
                    writeln('natural log of a');
                    writeln(b);
                    writeln();
                end;
            2 : begin
                    writeln('a is a perfect square');
                    writeln('natural log of a');
                    writeln(b);
                    writeln();
                end;
            3 : begin
                    writeln('a is a perfect square');
                    writeln('natural log of a');
                    writeln(b);
                    writeln();
                end;
        end;
        a := a + 1;
    end;
    a := 4;
    writeln(a);
    writeln(b); 
end.