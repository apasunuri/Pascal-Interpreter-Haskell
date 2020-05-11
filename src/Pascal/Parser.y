{
module Pascal.Parser where

import Pascal.Base
import Pascal.Data
import Pascal.Lexer
}


%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        int             { Token _ (TokenInt $$) }
        float           { Token _ (TokenFloat $$) }
        ID              { Token _ (TokenID $$)  }
        content         { Token _ (TokenContent $$) }
        '+'             { Token _ (TokenOp "+")   }
        '-'             { Token _ (TokenOp "-")   }
        '*'             { Token _ (TokenOp "*")   }
        '/'             { Token _ (TokenOp "/")   }
        '>'             { Token _ (TokenOp  ">")   }
        '<'             { Token _ (TokenOp  "<")   }
        '>='            { Token _ (TokenOp  ">=")   }
        '<='            { Token _ (TokenOp  "<=")   }
        '<>'            { Token _ (TokenOp  "<>")   }
        '='             { Token _ (TokenOp "=")   }
        ':='            { Token _ (TokenOp ":=")  }
        'sin'           { Token _ (TokenOp "sin") }
        'cos'           { Token _ (TokenOp "cos") }
        'sqrt'          { Token _ (TokenOp "sqrt") }
        'ln'            { Token _ (TokenOp "ln") }
        'exp'           { Token _ (TokenOp "exp") }
        'and'           { Token _ (TokenOp "and") }
        'or'            { Token _ (TokenOp "or") }
        'not'           { Token _ (TokenOp "not") }
        'writeln'       { Token _ (TokenOp "writeln") }
        'if'            { Token _ (TokenOp "if")   }
        'else'          { Token _ (TokenOp "else") }
        'case'          { Token _ (TokenOp "case") }
        'while'         { Token _ (TokenOp "while") }
        'for'           { Token _ (TokenOp "for") }
        'break'         { Token _ (TokenOp "break") }
        'continue'      { Token _ (TokenOp "continue") }
        '('             { Token _ (TokenK  "(")   }
        ')'             { Token _ (TokenK  ")")   }
        '.'             { Token _ (TokenK  ".")   }
        ':'             { Token _ (TokenK  ":")   }
        ';'             { Token _ (TokenK  ";")   }
        ','             { Token _ (TokenK  ",")   }
        '\''            { Token _ (TokenK  "'")   }
        '"'             { Token _ (TokenK  "\"")   }
        'program'       { Token _ (TokenK "program") }
        'begin'         { Token _ (TokenK "begin") }
        'end'           { Token _ (TokenK "end")  }
        'var'           { Token _ (TokenK "var")  }
        'then'          { Token _ (TokenK "then") }
        'of'            { Token _ (TokenK "of") }
        'to'            { Token _ (TokenK "to") }
        'do'            { Token _ (TokenK "do") }
        'function'      { Token _ (TokenK "function") }
        'procedure'     { Token _ (TokenK "procedure") }
        'true'          { Token _ (TokenK "true") }
        'false'         { Token _ (TokenK "false") }
        'real'          { Token _ (TokenK "real") }
        'boolean'         { Token _ (TokenK "boolean") }

-- associativity of operators in reverse precedence order
%nonassoc '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/'
%left 'sin' 'cos' 'exp' 'ln' 'sqrt'
%nonassoc ':='
%%

-- Entry point
Program :: {Program}
    : ProgramHeader Vars Functions 'begin' Statements 'end' '.' { $5, $2, [[]], $3 }

ProgramHeader :: {ProgramHeader}
    : 'program' ID ';' { Header $2 }

Vars :: {[Definition]}
    : 'var' Defs { $2 }
    | { [] }

Defs :: {[Definition]}
    : { [] }
    | Definition Defs { $1:$2 }

Definition :: {Definition}
    : ID_list ':' Type ';' { VarDef $1 $3 }

Type :: {VTypes}
    : 'boolean' { BOOLEAN }
    | 'real' { REAL }

ID_list :: {[String]}
    : ID {[$1]}
    | ID ',' ID_list { $1:$3 }

Functions :: {[Function]}
    : { [] }
    | Function ';' Functions { $1:$3 }

Function :: {Function}
    : 'function' ID '(' Params ')' ':' Type ';' Vars 'begin' Statements 'end' { Func $2 $4 $7 $9 $11 }
    | 'procedure' ID '(' Params ')' ';' Vars 'begin' Statements 'end' { Proc $2 $4 $7 $9 }
    | 'function' ID '(' ')' ':' Type ';' Vars 'begin' Statements 'end' { FuncNoParam $2 $6 $8 $10 }
    | 'procedure' ID '(' ')' ';' Vars 'begin' Statements 'end' { ProcNoParam $2 $6 $8 }

Params :: {Params}
    : ID_list ':' Type { Parameter $1 $3 }

-- Expressions
Exp :: {Exp}
    : '+' Exp { $2 } -- ignore Plus
    | '-' Exp { Op1 "-" $2}
    | 'sin' Exp { Op1 "sin" $2 }
    | 'cos' Exp { Op1 "cos" $2 }
    | 'sqrt' Exp { Op1 "sqrt" $2 }
    | 'ln' Exp { Op1 "ln" $2 }
    | 'exp' Exp { Op1 "exp" $2 }
    | Exp '-' Exp { Op2 "-" $1 $3 }
    | Exp '/' Exp { Op2 "/" $1 $3 }
    | Exp '+' Exp { Op2 "+" $1 $3 }
    | Exp '*' Exp { Op2 "*" $1 $3 }
    | '(' Exp ')' { $2 } -- ignore brackets
    | ID '(' Args ')' { FunCall $1 $3 }
    | float { Real $1 }
    | int { Integer $1 }
    | ID { Var $1 }

BoolExp :: {BoolExp}
    : 'true' { True_C }
    | 'false' { False_C }
    | 'not' BoolExp { Not $2 }
    | BoolExp 'and' BoolExp { OpB "and" $1 $3 }
    | BoolExp 'or' BoolExp { OpB "or" $1 $3 }
    | Exp '<' Exp { Comp "<" $1 $3 }
    | Exp '>' Exp { Comp ">" $1 $3 }
    | Exp '>=' Exp { Comp ">=" $1 $3 }
    | Exp '<=' Exp { Comp "<=" $1 $3 }
    | Exp '=' Exp { Comp "=" $1 $3 }
    | Exp '<>' Exp { Comp "<>" $1 $3 }
    | BoolExp '=' BoolExp { BoolComp "=" $1 $3 }
    | BoolExp '<>' BoolExp { BoolComp "<>" $1 $3 }
    | ID '(' Args ')' { BoolFunCall $1 $3 }
    | '(' BoolExp ')' { $2 }
    | ID { Var_B $1 } 

Args :: {[GenExp]}
    : { [] }
    | GenExp { [$1] }
    | GenExp ',' Args { $1:$3 }

Statements :: {[Statement]}
    : { [] } -- nothing; make empty list
    | Statement ';' Statements { $1:$3 } -- put statement as first element of statements

GenExp :: {GenExp}
    : Exp { FloatExp $1 }
    | BoolExp { BExp $1 }
    | ID { Variable $1 }
    | ID '(' Args ')' { Function $1 $3 }

WriteExp :: {WriteExp}
    : { Content "" }
    | content { Content $1 }
    | GenExp { GExp $1 }
    | '(' WriteExp ')' { $2 }

Options :: {[Option]}
    : { [] }
    | Option Options { $1:$2 } 

Option :: {Option}
    : GenExp ':' Statement ';' { Opt $1 $3 }

Statement :: {Statement}
    : ID ':=' GenExp { Assign $1 $3 }
    | 'writeln' WriteExp { Write $2 }
    | 'if' BoolExp 'then' Statement 'else' Statement   { If $2 $4 $6 }  
    | 'case' GenExp 'of' Options 'end' { Case $2 $4 }
    | Vars 'while' BoolExp 'do' Statement { While $1 $3 $5 }
    | Vars 'for' ID ':=' GenExp 'to' GenExp 'do' Statement { For $1 $3 $5 $7 $9 }
    | ID '(' Args ')' { ProcCall $1 $3 } 
    | 'begin' Statements 'end' { Block $2 }
    | 'break' { Break }
    | 'continue' { Continue }

{}
