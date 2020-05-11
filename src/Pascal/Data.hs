module Pascal.Data
    (
        ProgramHeader(..),
        Exp(..),
        BoolExp(..),
        Statement(..),
        GenExp(..),
        WriteExp(..),
        VTypes(..),
        Definition(..),
        Option(..),
        Function(..),
        Params(..),
        Program
    ) 
where

data ProgramHeader = 
    Header String

data Exp = 
    Op1 String Exp
    | Op2 String Exp Exp
    | FunCall String [GenExp]
    | Real Float
    | Integer Int
    | Var String

data BoolExp = 
    OpB String BoolExp BoolExp
    | Not BoolExp
    | Comp String Exp Exp
    | BoolComp String BoolExp BoolExp
    | BoolFunCall String [GenExp]
    | True_C
    | False_C
    | Var_B String

data GenExp = 
    FloatExp Exp
    | BExp BoolExp
    | Variable String
    | Function String [GenExp]

data WriteExp =
    Content String 
    | GExp GenExp

data Statement = 
    Assign String GenExp
    | Write WriteExp
    | If BoolExp Statement Statement
    | Case GenExp [Option]
    | While [Definition] BoolExp Statement
    | For [Definition] String GenExp GenExp Statement
    | ProcCall String [GenExp]
    | Break
    | Continue
    | Block [Statement]

data Option = 
    Opt GenExp Statement

data VTypes = REAL | BOOLEAN;

data Definition = 
    VarDef [String] VTypes

data Function = 
    Func String Params VTypes [Definition] [Statement]
    | Proc String Params [Definition] [Statement]
    | FuncNoParam String VTypes [Definition] [Statement]
    | ProcNoParam String [Definition] [Statement]

data Params = Parameter [String] VTypes

type Program = ([Statement], [Definition], [[(String, Either Float Bool)]], [Function])