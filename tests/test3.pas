program test3;

var
    temp_f, temp_c : real;

begin
    temp_f := 25;
    temp_c := (5 * (temp_f - 32)) / 9;
    writeln('The Weather in Fahrenheit');
    writeln(temp_f);
    writeln('Temperature in Celsius');
    writeln(temp_c);
    if(temp_c <= 0) then
        writeln('It is below freezing temperature.')
    else
        writeln('It is above freezing temperature.');
end.