module AST where

type VarName = String

data TinyL = TinyL BoolExpr [Stmt] BoolExpr BoolExpr 
           deriving (Show, Eq, Ord)

data Stmt = Attrib VarName IntExpr
          | If BoolExpr [Stmt]
          | TryCatch [Stmt] [Stmt]
          | Throw
          | IfThenElse BoolExpr [Stmt] [Stmt]
          | Loop BoolExpr BoolExpr [Stmt]
          deriving (Show, Eq, Ord)

data IntExpr = Var String
             | IntConst Integer
             | Neg IntExpr
             | IntBinary IntBinOp IntExpr IntExpr
             deriving (Show, Eq, Ord)


data BoolExpr = BoolConst Bool
              | Not BoolExpr
              | BoolBinary BoolBinOp BoolExpr BoolExpr
              | RelationalBinary RelBinOp IntExpr IntExpr
              deriving (Show, Eq, Ord)

data IntBinOp = Add
            | Sub
            | Mul
            | Div 
            deriving (Show, Eq, Ord)

data BoolBinOp = And
               | Or
               | Implies
               deriving (Show, Eq, Ord)

data RelBinOp = GT
              | LT
              | GTE
              | LTE
              | EQ
              | Diff
              deriving (Show, Eq, Ord)
