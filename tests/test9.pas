program test9;

var
   gs, cgs, e, gi, ni : real;

function calculateGrossIncome(grossSales, costOfGoodsSold : real) : real;
begin
    calculateGrossIncome := grossSales - costOfGoodsSold;
end;

function calculateNetIncome(grossIncome, expenses : real) : real;
begin
    calculateNetIncome := grossIncome - expenses;
end; 

procedure produceIncomeStatement();
begin
    writeln('Gross Sales $');
    writeln(gs);
    writeln('Cost of Goods that were Sold $');
    writeln(cgs);
    writeln('Corporate Expenses $');
    writeln(e);
    gi := calculateGrossIncome(gs, cgs);
    ni := calculateNetIncome(gi, e);
    writeln('Gross Income $');
    writeln(gi);
    writeln('Net Income $');
    writeln(ni);
end;  

begin
    gs := 100000;
    cgs := 40000;
    e := 10000;
    produceIncomeStatement();
end.