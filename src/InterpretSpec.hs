-- Look at how testing is set up in FORTH project and emulate here
-- Make sure you unit test every function you write

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Pascal.Interpret
import Pascal.Data
import Data.Maybe
import Data.Either

main :: IO ()
main = hspec $ do
    describe "biOp" $ do
        it "addition" $ do
            biOp "+" 2.3 4.3 `shouldBe` 6.6000004
        it "subtraction" $ do
            biOp "-" 2.3 1.0 `shouldBe` 1.3000000
        it "multiplication" $ do
            biOp "*" 2.3 1.0 `shouldBe` 2.3000000
        it "division" $ do
            biOp "/" 8.8 4 `shouldBe` 2.2
    describe "unOp" $ do
        it "positive" $ do
            unOp "+" 2.2 `shouldBe` 2.2
        it "negation" $ do
            unOp "-" 2.22 `shouldBe` -2.22
        it "sin" $ do
            unOp "sin" 5.7 `shouldBe` -0.5506857
        it "cos" $ do
            unOp "cos" 78.0 `shouldBe` -0.8578030932449878
        it "sqrt" $ do
            unOp "sqrt" 64.0 `shouldBe` 8.0
        it "ln" $ do
            unOp "ln" 5.88 `shouldBe` 1.7715567619105355
        it "exp" $ do
            unOp "exp" 5.7 `shouldBe` 298.86734
    describe "biBoolOp" $ do
        it "and" $ do 
            biBoolOp "and" True False `shouldBe` False
        it "or" $ do 
            biBoolOp "or" True False `shouldBe` True
    describe "realCompOp" $ do
        it "greater than" $ do 
            realCompOp ">" 22.2 8.0 `shouldBe` True
        it "less than" $ do 
            realCompOp "<" 22.2 8.0 `shouldBe` False
        it "greater than or equal" $ do 
            realCompOp ">=" 1.0 1.0 `shouldBe` True
        it "less than or equal" $ do 
            realCompOp "<=" 1.0 1.0 `shouldBe` True
        it "equal to" $ do 
            realCompOp "=" 6.7 8.0 `shouldBe` False
        it "not equal to" $ do 
            realCompOp "<>" 22.2 8.0 `shouldBe` True
    describe "boolCompOp" $ do
        it "equal to" $ do 
            boolCompOp "=" True False `shouldBe` False
        it "not equal to" $ do 
            boolCompOp "<>" True False `shouldBe` True
    describe "function" $ do
        it "function" $ do
            Pascal.Interpret.function "add" [(FloatExp (Integer 3)), (FloatExp (Integer 5))] [[]] [(Func "add" (Parameter ["a", "b"] REAL) REAL [VarDef ["add"] REAL] [(Assign "add" (FloatExp (Op2 "+" (Var "a") (Var "b"))))])] [] False `shouldBe` (Left 8)
    describe "functionEval" $ do
        it "empty function evaluation" $ do
            functionEval (Func "add" (Parameter ["a", "b"] REAL) REAL [] []) [] [] [] False `shouldBe` ("", [])
        it "empty function evaluation with no parameters" $ do
            functionEval (FuncNoParam "add" REAL [] []) [] [] [] False `shouldBe` ("", [])
        it "function evaluation" $ do
            functionEval (Func "add" (Parameter ["a", "b"] REAL) REAL [] []) [[]] [(Func "add" (Parameter ["a", "b"] REAL) REAL [VarDef ["add"] REAL] [(Assign "add" (FloatExp (Op2 "+" (Var "a") (Var "b"))))])] [] False `shouldBe` ("", [[]])
        it "function evaluation with no parameters" $ do
            functionEval (FuncNoParam "add" REAL [] []) [] [(FuncNoParam "add" REAL [] [(Assign "add" (FloatExp (Op2 "+" (Var "a") (Var "b"))))])] [] False `shouldBe` ("", []) 
    describe "getArgs" $ do
        it "no args" $ do
            getArgs [] (ProcNoParam "add" [] [(Write (GExp (FloatExp(Op2 "+" (Integer 1) (Integer 1)))))]) [] [] [] False `shouldBe` []
            getArgs [(FloatExp (Integer 3)), (FloatExp (Integer 5))] (Func "add" (Parameter ["a", "b"] REAL) REAL [] [(Assign "add" (FloatExp (Op2 "+" (Var "a") (Var "b"))))]) [[]] [] [] False `shouldBe` [("a", Left 3), ("b", Left 5)]
            getArgs [(FloatExp (Integer 3)), (FloatExp (Integer 5))] (Proc "add" (Parameter ["a", "b"] REAL) [] [(Write (GExp((FloatExp (Op2 "+" (Var "a") (Var "b"))))))]) [[]] [] [] False `shouldBe` [("a", Left 3), ("b", Left 5)]
    describe "isFunction" $ do
        it "determine if it is a function" $ do
            isFunction "add" (Func "add" (Parameter ["a", "b"] REAL) REAL [] [(Write (GExp(FloatExp(Op2 "+" (Var "a") (Var "b")))))]) `shouldBe` True
            isFunction "add" (Proc "add" (Parameter ["a", "b"] REAL) [] [(Write (GExp(FloatExp(Op2 "+" (Var "a") (Var "b")))))]) `shouldBe` True
            isFunction "add" (FuncNoParam "add" REAL [] [(Write (GExp(FloatExp(Op2 "+" (Real 1.4) (Real 5.6)))))]) `shouldBe` True
            isFunction "add" (ProcNoParam "add" [] [(Write (GExp(FloatExp(Op2 "+" (Real 1.4) (Real 5.6)))))]) `shouldBe` True
    describe "realExp" $ do
        it "binary operator" $ do 
            realExp (Op2 "+" (Real 3.14) (Integer 8)) [] [] [] False `shouldBe` 11.14
        it "unary operator" $ do 
            realExp (Op1 "-" (Integer 8)) [] [] [] False `shouldBe` -8.0
        it "function call" $ do
            realExp (FunCall "add" [FloatExp(Real 4.5), FloatExp(Real 4.5)]) [[]] [(Func "add" (Parameter ["num1", "num2"] REAL) REAL [] [(Assign "add" (FloatExp (Op2 "+" (Var "num1") (Var "num2"))))])] [] False `shouldBe` 9.0
        it "real" $ do 
            realExp (Real 4.56) [] [] [] False `shouldBe` 4.56
        it "integer" $ do 
            realExp (Integer 8) [] [] [] False `shouldBe` 8.0
        it "variable" $ do 
            realExp (Var "a") [[("a", Left 4.5::Either Float Bool)]] [] [VarDef ["a"] REAL] False `shouldBe` 4.5
    describe "boolExp" $ do
        it "binary operator" $ do
            boolExp (OpB "and" (True_C) (True_C)) [] [] [] False `shouldBe` True
        it "not operator" $ do
            boolExp (Not (True_C)) [] [] [] False `shouldBe` False
        it "real comparator operator" $ do
            boolExp (Comp ">" (Real 6.7) (Integer 5)) [] [] [] False `shouldBe` True
        it "boolean comparator operator" $ do
            boolExp (BoolComp "<>" (True_C) (False_C)) [] [] [] False `shouldBe` True
        it "boolean function call" $ do
            boolExp (BoolFunCall "and" [BExp(True_C), BExp(True_C)]) [[]] [(Func "and" (Parameter ["a", "b"] BOOLEAN) BOOLEAN [] [(Assign "and" (BExp (OpB "and" (Var_B "a") (Var_B "b"))))])] [] False `shouldBe` True
        it "true value" $ do
            boolExp (True_C) [] [] [] False `shouldBe` True
        it "false value" $ do
            boolExp (False_C) [] [] [] False `shouldBe` False
        it "boolean variable" $ do 
            boolExp (Var_B "a") [[("a", Right True::Either Float Bool)]] [] [VarDef ["a"] BOOLEAN] False `shouldBe` True
    describe "assignExp" $ do
        it "assign expression" $ do
            assignExp "a" (FloatExp (Real 3.14)) [[]] [] [VarDef ["a"] REAL] False `shouldBe` ("", [[("a", Left 3.14)]])
    describe "assign" $ do
        it "assign" $ do
            assign "a" ("a", Right True) [("a", Right False), ("b", Left 4.8)] `shouldBe` [("a", Right True), ("b", Left 4.8)]
    describe "varLookup" $ do
        it "empty list" $ do
            varLookup "a" [] False `shouldBe` Nothing
        it "lookup variables" $ do
            varLookup "a" [[("a", Right True), ("b", Left 5.3)]] False `shouldBe` Just (Right True)
    describe "countVal" $ do
        it "no definitions" $ do
            countVal "a" [] `shouldBe` 0
        it "count variable definitions" $ do
            countVal "a" [VarDef ["a"] REAL, VarDef ["b", "c"] BOOLEAN, VarDef ["a"] REAL] `shouldBe` 2
    describe "count" $ do
        it "count list with not elements" $ do
            count "a" [] `shouldBe` 0
        it "count elements in list" $ do
            count "a" ["a", "c", "a", "d", "a", "a"] `shouldBe` 4
    describe "writeExp" $ do
        it "write content" $ do
            writeExp (Content "'Pascal Program'") [] [] [] False `shouldBe` ("Pascal Program\n", [])
        it "write general expression" $ do
            writeExp (GExp (FloatExp (Op1 "ln" (Real 4.5)))) [] [] [] False `shouldBe` ("1.5040774\n", []) 
    describe "ifExp" $ do
        it "if statements" $ do
            ifExp (Comp "<" (Integer 5) (Integer 7)) (Write (Content "'Hello World'")) (Write (Content "'Goodbye World'")) [] [] [] False `shouldBe` ("Hello World\n", [])
    describe "caseExp" $ do
        it "case statements" $ do
            caseExp (FloatExp (Op1 "sqrt" (Real 64.0))) [(Opt (FloatExp (Real 8.0)) (Write (Content ("'Value is a Perfect Square'")))), (Opt (FloatExp (Real 7.0)) (Write (Content ("'Value is not a Perfect Square'"))))] [] [] [] False `shouldBe` ("Value is a Perfect Square\n", []) 
    describe "evaluateCase" $ do
        it "case evaluation" $ do
            evaluateCase (Opt (FloatExp (Op2 "+" (Integer 5) (Integer 3))) (Write (Content ("'Value'")))) [] [] [] False `shouldBe` ("Value\n", [])
    describe "isCase" $ do
        it "case match" $ do
            isCase (FloatExp (Integer 5)) (Opt (FloatExp (Integer 5)) (Write (Content "'Value'"))) [] [] [] False `shouldBe` True
    describe "whileExp" $ do
        it "while loop" $ do
            whileExp (Comp "<" (Var "a") (Integer 1)) (Block [(Write (Content "'Hello World'")), (Assign "a" (FloatExp (Op2 "+" (Var "a") (Integer 1))))]) [[("a", Left 0)]] [] [VarDef ["a"] REAL] False `shouldBe` ("Hello World\n", [])
    describe "forLoop" $ do
        it "for loop" $ do
            forExp "a" (FloatExp (Integer 0)) (FloatExp (Integer 1)) 0 (Write (Content "'value'")) [[("a", Left 0)], []] [] [VarDef ["a"] REAL] False `shouldBe` ("value\nvalue\n", [[("a", Left 1)],[]]) 
    describe "removeBreak" $ do
        it "remove break" $ do
            removeBreak "BREAKhello" `shouldBe` "hello"
    describe "break" $ do
        it "break eval" $ do
            Pascal.Interpret.break [] [] [] False `shouldBe` ("BREAK", [])
    describe "continue" $ do
        it "continue eval" $ do
            continue [] [] [] False `shouldBe` ("CONTINUE", [])
    describe "blockExp" $ do
        it "empty block expression" $ do
            blockExp [] [] [] [] False `shouldBe` ("", [])
        it "block expression" $ do
            blockExp [(Write (Content "'Hello World'")), (Write (Content "'It's a good day'"))] [] [] [] False `shouldBe` ("Hello World\nIt's a good day\n", [])
    describe "procedure" $ do
        it "procedure expression" $ do
            procedure "add" [(FloatExp (Integer 8)), (FloatExp (Integer 8))] [[]] [(Proc "add" (Parameter ["a", "b"] REAL) [] [(Write (GExp(FloatExp (Op2 "+" (Var "a") (Var "b")))))])] [] True `shouldBe` ("16.0\n", [])
    describe "procedureEval" $ do
        it "procedure no statements" $ do
            procedureEval (Proc "add" (Parameter ["a", "b"] REAL) [] []) [[]] [] [] False `shouldBe` ("", [])
        it "procedure no statements and no arguments" $ do    
            procedureEval (ProcNoParam "add" [] []) [[]] [] [] False `shouldBe` ("", [])
        it "procedure evaluation" $ do
            procedureEval (Proc "add" (Parameter ["a", "b"] REAL) [] [(Write (GExp(FloatExp (Op2 "+" (Integer 1) (Integer 1)))))]) [[], []] [] [(VarDef ["a", "b"] REAL)] True `shouldBe` ("2.0\n", [[]])
        it "procedure evaluation no parameters" $ do
            procedureEval (ProcNoParam "add" [] [(Write (GExp(FloatExp (Op2 "+" (Integer 1) (Integer 1)))))]) [[]] [] [] True `shouldBe` ("2.0\n", [])
    describe "evaluate" $ do
        it "assign" $ do
            Pascal.Interpret.evaluate (Assign "a" (FloatExp (Real 4.5))) [[]] [] [VarDef ["a"] REAL] False `shouldBe` ("", [[("a", Left 4.5)]]) 
        it "write" $ do
            Pascal.Interpret.evaluate (Write (Content "'hello world'")) [[]] [] [] False `shouldBe` ("hello world\n", [[]])
        it "if" $ do
            Pascal.Interpret.evaluate (If (True_C) (Write (Content "'true'")) (Write (Content "'false'"))) [[]] [] [] False `shouldBe` ("true\n", [[]])
        it "case" $ do
            Pascal.Interpret.evaluate (Case (FloatExp (Integer 1)) [(Opt (FloatExp (Integer 1)) (Write (Content "'true'")))]) [] [] [] False `shouldBe` ("true\n", [])
        it "while loop" $ do
            Pascal.Interpret.evaluate (While [] (Comp "<" (Var "a") (Integer 1)) (Block [(Write (Content "'iteration'")), (Assign "a" (FloatExp (Op2 "+" (Var "a") (Integer 1))))])) [[("a", Left 0)]] [] [VarDef ["a"] REAL] False `shouldBe` ("iteration\n", [[("a", Left 1.0)]])
        it "for loop" $ do
            Pascal.Interpret.evaluate (For [] "a" (FloatExp (Integer 0)) (FloatExp (Integer 1)) (Write (Content "'hello world'"))) [[]] [] [VarDef ["a"] REAL] False `shouldBe` ("hello world\nhello world\n", [[("a", Left 1)], []])
        it "break" $ do
            Pascal.Interpret.evaluate (Break) [] [] [] False `shouldBe` ("BREAK", [])
        it "continue" $ do
            Pascal.Interpret.evaluate (Continue) [] [] [] False `shouldBe` ("CONTINUE", [])
        it "procedure call" $ do
            Pascal.Interpret.evaluate (ProcCall "add" [(FloatExp (Integer 1)), (FloatExp (Integer 1))]) [[]] [(Proc "add" (Parameter ["a", "b"] REAL) [] [(Write (GExp(FloatExp(Op2 "+" (Var "a") (Var "b")))))])] [] True `shouldBe` ("2.0\n", [[]])
        it "block" $ do
            Pascal.Interpret.evaluate (Block [(Write (Content "'hello world'"))]) [] [] [] False `shouldBe` ("hello world\n", [])
    describe "genExp" $ do
        it "function" $ do
            genExp (Function "add" [FloatExp(Real 2.22), FloatExp(Real 2.22)]) [[]] [(Func "add" (Parameter ["num1", "num2"] REAL) REAL [] [(Assign "add" (FloatExp (Op2 "+" (Var "num1") (Var "num2"))))])] [] False `shouldBe` (Left 4.44::Either Float Bool)
        it "variable" $ do
            genExp (Variable "a") [[("a", Left 8.8::Either Float Bool)]] [] [] False `shouldBe` (Left 8.8::Either Float Bool) 
        it "float expression" $ do
            genExp (FloatExp (Real 6.78)) [] [] [] False `shouldBe` (Left 6.78::Either Float Bool) 
        it "boolean expression" $ do
            genExp (BExp (False_C)) [] [] [] False `shouldBe` (Right False::Either Float Bool) 
    describe "contains" $ do
        it "no variables" $ do
            contains "a" [] `shouldBe` False
        it "variables" $ do    
            contains "a" [VarDef ["a"] BOOLEAN] `shouldBe` True
    describe "interpret" $ do
        it "empty program" $ do
            interpret ([], [], [], []) `shouldBe` ""
        it "write" $ do
            interpret ([Write (Content("'hello world'"))], [], [], []) `shouldBe` "hello world\n"