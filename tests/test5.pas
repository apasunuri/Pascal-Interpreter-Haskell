program test5;

var
    a, b, c, sum: real;

begin
    a := 8;
    b := 6;
    while a < 16 do
        begin
            a := a + 1;
            if(a > 10) then 
                continue
            else
                writeln(a); 
        end;
    writeln();
    for c := 0 to b do
        begin
            if(c > 4) then
                break
            else
                writeln(c);
        end;
end.