module TinyL where

import AST
import Grammar
import VCGen
import qualified Data.Set as Set
import Z3.Monad
import Control.Monad.Trans

lstPrettyPrint:: [BoolExpr] -> String
lstPrettyPrint [] = ""
lstPrettyPrint (x:xs) = "\n" ++ prettyPrint x ++ "\n" ++ lstPrettyPrint xs

prettyPrint :: BoolExpr -> String
prettyPrint (BoolConst b) = show b
prettyPrint (Not exp) = "! (" ++ prettyPrint exp ++ " )"
prettyPrint (BoolBinary op exp1 exp2) = prettyPrint exp1 ++ ppBoolOp op ++ prettyPrint exp2
prettyPrint (RelationalBinary op exp1 exp2) = ppIntExpr exp1 ++ ppRelOp op ++ ppIntExpr exp2

ppIntExpr :: IntExpr -> String
ppIntExpr (Var s) = s
ppIntExpr (IntConst i) = show i
ppIntExpr (Neg exp) = "- " ++ ppIntExpr exp
ppIntExpr (IntBinary op exp1 exp2) = ppIntExpr exp1 ++ ppIntOp op ++ ppIntExpr exp2

ppBoolOp :: BoolBinOp -> String
ppBoolOp And = " && "
ppBoolOp Or = " || "
ppBoolOp Implies = " ==> "

ppIntOp :: IntBinOp -> String
ppIntOp Add = " + "
ppIntOp Sub = " - "
ppIntOp Mul = " * "
ppIntOp Div = " / "

ppRelOp :: RelBinOp -> String
ppRelOp AST.GT = " > "
ppRelOp AST.LT = " < "
ppRelOp AST.GTE = " >= "
ppRelOp AST.LTE = " <= "
ppRelOp AST.EQ = " == "
ppRelOp AST.Diff = " != "

script :: TinyL -> Z3 [Result]
script x = do
    vc <- vcs x
    mapM (\l -> reset >> assert l >> check) vc

f Sat = "Invalid"
f Unsat = "Valid"
f Undef = "Undef"

g x = map f x

h [] = ""
h (x:xs) = x ++ "\n" ++ h xs

main :: IO ()
main = do 
    putStrLn "Nome do ficheiro a dar parse:"
    f <- getLine
    tiny <- parseFile f
    putStrLn $ lstPrettyPrint (Set.toList $ vcg tiny)

    result <- evalZ3 $ script tiny

    putStrLn $ (h.g) result