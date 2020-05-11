program test7;

var
    output: real;

function factorial(num: real): real;
begin
    if(num <= 0) then
        factorial := 1
    else
        factorial := num * factorial(num - 1);
end;

begin 
    output := factorial(5);
    writeln(output);
end.