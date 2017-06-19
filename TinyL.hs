module TinyL where

type VarName = String

data TinyL = TinyL Acsl [Stmt] Acsl Acsl 
           deriving Show

data Acsl = Old VarName RelBinOp IntExpr
          | Prop BoolExpr
          deriving Show

data Stmt = Attrib VarName IntExpr
          | If BoolExpr [Stmt]
          | TryCatch [Stmt] [Stmt]
          | Throw
          | IfThenElse BoolExpr [Stmt] [Stmt]
          | Loop BoolExpr Acsl [Stmt]
          deriving Show

data IntExpr = Var String
             | IntConst Integer
             | Neg IntExpr
             | IntBinary IntBinOp IntExpr IntExpr
             deriving Show


data BoolExpr = BoolConst Bool
              | Not BoolExpr
              | BoolBinary BoolBinOp BoolExpr BoolExpr
              | RelationalBinary RelBinOp IntExpr IntExpr
              deriving Show

data IntBinOp = Add
            | Sub
            | Mul
            | Div 
            deriving Show

data BoolBinOp = And
               | Or
               deriving Show

data RelBinOp = GT
              | LT
              | GTE
              | LTE
              | EQ
              | Diff
              deriving Show
