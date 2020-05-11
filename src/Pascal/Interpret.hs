module Pascal.Interpret 
(
    biOp,
    unOp,
    biBoolOp,
    realCompOp,
    boolCompOp,
    function,
    functionEval,
    getArgs,
    getFunction,
    isFunction,
    realExp,
    boolExp,
    genExp,
    assignExp,
    varLookup,
    assign,
    countVal,
    count,
    contains,
    writeExp,
    ifExp,
    caseExp,
    evaluateCase,
    getCase,
    isCase,
    whileExp,
    forExp,
    removeBreak,
    Pascal.Interpret.break,
    continue,
    blockExp,
    procedure,
    procedureEval,
    evaluate,
    interpret
)
where

import Pascal.Data
import Data.Either
import Data.Maybe
import Data.List

biOp :: String -> Float -> Float -> Float
biOp "+" v1 v2 = v1 + v2
biOp "-" v1 v2 = v1 - v2
biOp "/" v1 v2 = v1 / v2
biOp "*" v1 v2 = v1 * v2

unOp :: String -> Float -> Float
unOp "-" v1 = -v1
unOp "+" v1 = v1
unOp "sin" v1 = sin v1
unOp "cos" v1 = cos v1
unOp "sqrt" v1 = sqrt v1
unOp "ln" v1 = log v1
unOp "exp" v1 = exp v1

biBoolOp :: String -> Bool -> Bool -> Bool
biBoolOp "and" v1 v2 = v1 && v2
biBoolOp "or" v1 v2 = v1 || v2

realCompOp :: String -> Float -> Float -> Bool
realCompOp ">" v1 v2 = v1 > v2
realCompOp "<" v1 v2 = v1 < v2
realCompOp ">=" v1 v2 = v1 >= v2
realCompOp "<=" v1 v2 = v1 <= v2
realCompOp "=" v1 v2 = v1 == v2
realCompOp "<>" v1 v2 = v1 /= v2

boolCompOp :: String -> Bool -> Bool -> Bool
boolCompOp "=" v1 v2 = v1 == v2
boolCompOp "<>" v1 v2 = v1 /= v2

function :: String -> [GenExp] -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Either Float Bool
function s1 e1 (a1 : a) f d b = fromJust (varLookup s1 (snd (functionEval (getFunction s1 f) (((getArgs e1 (getFunction s1 f) (a1 : a) f d b) ++ a1) : a) f d b)) b)

functionEval :: Function -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
functionEval (Func s1 (Parameter s2 t1) t2 d []) a f d1 b = ("", a)
functionEval (FuncNoParam s1 t2 d []) a f d1 b = ("", a)
functionEval (Func s1 (Parameter s2 t1) t2 d (s:st)) a f d1 b = (fst (evaluate s a f (d ++ d1) b) ++ fst (functionEval (Func s1 (Parameter s2 t1) t2 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b), snd (functionEval (Func s1 (Parameter s2 t1) t2 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b))
functionEval (FuncNoParam s1 t2 d (s:st)) a f d1 b = (fst (evaluate s a f (d ++ d1) b) ++ fst (functionEval (FuncNoParam s1 t2 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b), snd (functionEval (FuncNoParam s1 t2 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b))

getArgs :: [GenExp] -> Function -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> [(String, Either Float Bool)]
getArgs [] f1 a f d b = []
getArgs (e:e1) (Func s1 (Parameter (s:s2) t1) t2 d st) a f d1 b = (s, genExp e a f d1 b) : getArgs e1 (Func s1 (Parameter s2 t1) t2 d st) a f d1 b
getArgs (e:e1) (Proc s1 (Parameter (s:s2) t1) d st) a f d1 b = (s, genExp e a f d1 b) : getArgs e1 (Proc s1 (Parameter s2 t1) d st) a f d1 b

getFunction :: String -> [Function] -> Function
getFunction s1 (f1:f) = if isFunction s1 f1 then f1 else getFunction s1 f

isFunction :: String -> Function -> Bool
isFunction s1 (Func s2 (Parameter s3 t1) t2 d st) = if s1 == s2 then True else False
isFunction s1 (Proc s2 (Parameter s3 t1) d st) = if s1 == s2 then True else False
isFunction s1 (FuncNoParam s2 t2 d st) = if s1 == s2 then True else False
isFunction s1 (ProcNoParam s2 d st) = if s1 == s2 then True else False

realExp :: Exp -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Float
realExp (Op2 op e1 e2) a f d b = biOp op (realExp e1 a f d b) (realExp e2 a f d b)
realExp (Op1 op e1) a f d b = unOp op (realExp e1 a f d b)
realExp (FunCall s1 e1) a f d b = if isLeft (function s1 e1 ([] : a) f d False) then fromLeft 0 (function s1 e1 ([] : a) f d False) else error("Invalid Type")
realExp (Real e1) a f d b = e1
realExp (Integer e1) a f d b = fromIntegral e1
realExp (Var s1) a f d b = fromLeft 0 (fromJust (varLookup s1 a b))

boolExp :: BoolExp -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Bool
boolExp (OpB op b1 b2) a f d b = biBoolOp op (boolExp b1 a f d b) (boolExp b2 a f d b)
boolExp (Not b1) a f d b = not (boolExp b1 a f d b)
boolExp (Comp op e1 e2) a f d b = realCompOp op (realExp e1 a f d b) (realExp e2 a f d b)
boolExp (BoolComp op b1 b2) a f d b = boolCompOp op (boolExp b1 a f d b) (boolExp b2 a f d b)
boolExp (BoolFunCall s1 e1) a f d b = if isRight (function s1 e1 ([] : a) f d False) then fromRight False (function s1 e1 ([] : a) f d False) else error("Invalid type")
boolExp (True_C) a f d b = True
boolExp (False_C) a f d b = False
boolExp (Var_B s1) a f d b = fromRight False (fromJust (varLookup s1 a b))

genExp :: GenExp -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Either Float Bool
genExp (Function s1 e1) a f d b = function s1 e1 ([] : a) f d False
genExp (Variable s1) a f d b = fromJust (varLookup s1 a b)
genExp (FloatExp e1) a f d b = Left (realExp e1 a f d b)
genExp (BExp b1) a f d b = Right (boolExp b1 a f d b)

varLookup :: String -> [[(String, Either Float Bool)]] -> Bool -> Maybe (Either Float Bool)
varLookup s1 [] b = Nothing
varLookup s1 (a1 : a) b = if b then (if (lookup s1 a1 /= Nothing) then (lookup s1 a1) else (lookup s1 (last a))) else (if (lookup s1 a1 /= Nothing) then (lookup s1 a1) else (varLookup s1 a b))

assignExp :: String -> GenExp -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
assignExp s1 e1 a f d b = if length a == 1 then (if contains s1 d then ("", map (\a1 -> assign s1 (s1, genExp e1 a f d b) a1) a) else error("Not Declared")) else (if (countVal s1 d) > 1 then ("", (assign s1 (s1, genExp e1 a f d b) (head a)) : (delete (head a) a)) else if (contains s1 (snd(splitAt ((length d) - 1) d))) then ("", map (\a1 -> assign s1 (s1, genExp e1 a f d b) a1) a) else ("", (assign s1 (s1, genExp e1 a f d b) (head a)) : (delete (head a) a)))

assign :: String -> (String, Either Float Bool) -> [(String, Either Float Bool)] -> [(String, Either Float Bool)]
assign s1 s a1 = (s : (filter (\x -> (fst x) /= s1) a1))

countVal :: String -> [Definition] -> Int
countVal s1 [] = 0
countVal s1 ((VarDef s t) : d) = (count s1 s) + (countVal s1 d)

count :: String -> [String] -> Int
count s1 [] = 0
count s1 (s : st) = if s1 == s then 1 + (count s1 st) else (count s1 st)

contains :: String -> [Definition] -> Bool
contains s1 [] = False
contains s1 ((VarDef s t) : d) = if (elemIndex s1 s) /= Nothing then True else contains s1 d 

writeExp :: WriteExp -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
writeExp (Content s1) a f d b = ((drop 1 (take (length s1 - 1) s1)) ++ "\n", a)
writeExp (GExp e1) a f d b = if isLeft (genExp e1 a f d b) then (show (fromLeft 0 (genExp e1 a f d b)) ++ "\n", a)
                       else (show (fromRight False (genExp e1 a f d b)) ++ "\n", a) 

ifExp :: BoolExp -> Statement -> Statement -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
ifExp b1 e1 e2 a f d b = if (boolExp b1 a f d b) then evaluate e1 a f d b else evaluate e2 a f d b

caseExp :: GenExp -> [Option] -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
caseExp e1 o1 a f d b = if isJust (getCase e1 o1 a f d b) then evaluateCase (fromJust (getCase e1 o1 a f d b)) a f d b else ("", a)

evaluateCase :: Option -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
evaluateCase (Opt e s) a f d b = evaluate s a f d b

getCase :: GenExp -> [Option] -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Maybe Option
getCase e1 [] a f d b = Nothing
getCase e1 (o:o1) a f d b = if isCase e1 o a f d b then (Just o) else getCase e1 o1 a f d b

isCase :: GenExp -> Option -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> Bool
isCase e1 (Opt e s) a f d b = (genExp e1 a f d b) == (genExp e a f d b)

whileExp :: BoolExp -> Statement -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
whileExp b1 e1 a f d b = if(boolExp b1 a f d b) then (if "BREAK" `isInfixOf` fst (evaluate e1 a f d b) then (removeBreak(fst (evaluate e1 a f d b)), delete (head a) a) else if "CONTINUE" `isInfixOf` fst (evaluate e1 a f d b) then (fst (whileExp b1 e1 (snd (evaluate e1 a f d b)) f d b), snd (whileExp b1 e1 (snd (evaluate e1 a f d b)) f d b)) else (fst (evaluate e1 a f d b) ++ fst(whileExp b1 e1 (snd(evaluate e1 a f d b)) f d b), snd (whileExp b1 e1 (snd (evaluate e1 a f d b)) f d b))) else ("", delete (head a) a)

forExp :: String -> GenExp -> GenExp -> Int -> Statement -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
forExp s1 e1 e2 i e (a1 : a) f d b = if (floor (fromLeft 0 (genExp e2 (a1 : a) f d b))) - (floor (fromLeft 0 (genExp e1 (a1 : a) f d b))) + 1 /= i then (if "BREAK" `isInfixOf` fst (evaluate e (a1 : a) f d b) then (removeBreak (fst (evaluate e (a1 : a) f d b)), a) else if "CONTINUE" `isInfixOf` fst (evaluate e (a1 : a) f d b) then (fst (forExp s1 e1 e2 (i + 1) e (((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 (a1 : a) f d b)) + (i + 1))) :: Either Float Bool) : (filter (\x -> (fst x) /= s1) (head (snd (evaluate e (a1 : a) f d b))))) : a) f d b), snd (forExp s1 e1 e2 (i + 1) e (((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 (a1 : a) f d b)) + (i + 1))) :: Either Float Bool) : (filter (\x -> (fst x) /= s1) (head (snd (evaluate e (a1 : a) f d b))))) : a) f d b)) else (fst (evaluate e (a1 : a) f d b) ++ fst (forExp s1 e1 e2 (i + 1) e (((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 (a1 : a) f d b)) + (i + 1))) :: Either Float Bool) : (filter (\x -> (fst x) /= s1) (head (snd (evaluate e (a1 : a) f d b))))) : a) f d b), (((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 (a1 : a) f d b)) + (i + 1))) :: Either Float Bool) : (filter (\x -> (fst x) /= s1) (head (snd (forExp s1 e1 e2 (i + 1) e (((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 (a1 : a) f d b)) + (i + 1))) :: Either Float Bool) : (filter (\x -> (fst x) /= s1) (head((snd (evaluate e (a1 : a) f d b)))))) : a) f d b))))) : a))) else ("", a)

removeBreak :: String -> String
removeBreak b = delete 'B' (delete 'R' (delete 'E' (delete 'A' (delete 'K' b))))

break :: [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
break a f d b = ("BREAK", a)

continue :: [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
continue a f d b = ("CONTINUE", a)

blockExp :: [Statement] -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
blockExp [] a f d b = ("", a)
blockExp (e:e1) a f d b = if ("BREAK" `isInfixOf` fst (evaluate e a f d b)) || ("CONTINUE" `isInfixOf` fst (evaluate e a f d b)) then (fst (evaluate e a f d b), a) else (fst (evaluate e a f d b) ++ fst (blockExp e1 (snd (evaluate e a f d b)) f d b), snd (blockExp e1 (snd (evaluate e a f d b)) f d b)) -- snd (blockExp e1 (fst (evaluate e a f) ++ s, snd (evaluate e a f)) f)

procedure :: String -> [GenExp] -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
procedure s1 e1 (a1 : a) f d b = procedureEval (getFunction s1 f) (((getArgs e1 (getFunction s1 f) (a1 : a) f d b) ++ a1) : a) f d b

procedureEval :: Function -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]])
procedureEval (Proc s1 (Parameter s2 t1) d []) a f d1 b = ("", delete (head a) a)
procedureEval (ProcNoParam s1 d []) a f d1 b = ("", delete (head a) a)
procedureEval (Proc s1 (Parameter s2 t1) d (s:st)) a f d1 b = (fst (evaluate s a f (d ++ d1) b) ++ fst(procedureEval (Proc s1 (Parameter s2 t1) d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b), snd (procedureEval (Proc s1 (Parameter s2 t1) d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b))
procedureEval (ProcNoParam s1 d (s:st)) a f d1 b = (fst (evaluate s a f (d ++ d1) b) ++ fst(procedureEval (ProcNoParam s1 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b), snd (procedureEval (ProcNoParam s1 d st) (snd (evaluate s a f (d ++ d1) b)) f d1 b))

evaluate :: Statement -> [[(String, Either Float Bool)]] -> [Function] -> [Definition] -> Bool -> (String, [[(String, Either Float Bool)]]) 
evaluate (Assign s1 e1) a f d b = assignExp s1 e1 a f d b
evaluate (Write e1) a f d b = writeExp e1 a f d b
evaluate (If b1 e1 e2) a f d b = ifExp b1 e1 e2 a f d b
evaluate (Case e1 o1) a f d b = caseExp e1 o1 a f d b
evaluate (While v b1 e1) a f d b = whileExp b1 e1 ([] : a) f (v ++ d) b
evaluate (For v s1 e1 e2 e3) a f d b = forExp s1 e1 e2 0 e3 ((((s1, Left (fromIntegral (floor (fromLeft 0 (genExp e1 a f d b)))) :: Either Float Bool) : [])) : a) f (v ++ d) b
evaluate (Break) a f d b = Pascal.Interpret.break a f d b
evaluate (Continue) a f d b = continue a f d b
evaluate (ProcCall s1 e1) a f d b = procedure s1 e1 ([] : a) f d True
evaluate (Block e1) a f d b = blockExp e1 a f d b

interpret :: Program -> String
interpret ([], d, a, f) = ""
interpret (s:p, d, a, f) = fst (evaluate s a f d False) ++ (interpret (p, d, snd (evaluate s a f d False), f))