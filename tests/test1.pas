program test1;

var
    x, y: real;

begin
    if(5 <> 7) then
        begin
            writeln(5 / 7);
            writeln('Valid Statement');
        end
    else
        writeln('Not a Valid Statement');
    if(5 > 7) then
        writeln(sqrt(5 / 7))
    else
        writeln('Not a Valid Statement');
end.