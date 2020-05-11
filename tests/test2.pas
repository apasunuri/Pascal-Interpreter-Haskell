program test2;

var
    a : real;
    b : boolean;

begin 
    a := 1.57;
    b := false;
    case(2 * a) of 
        4.5 : begin
                writeln(sqrt(4.5));
                writeln('Real Number');
              end;
        1.25 : begin
                writeln(sqrt(1.25));
                writeln('Another Real Number');
              end;
        3.14 : writeln('PI');
        2.22 : writeln('Another Real Number');
    end;
    case(b) of 
        true : writeln('Hello World');
        false : writeln('Goodbye World');
    end;
end.