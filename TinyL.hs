module TinyL where
{-
Árvore de Sintaxe Abstrata da TinyLanguage a ser desenvolvida.
Falta a incorporação de Anotações
-}
type VarName = String

--Pré condição e Pós-condições como BoolExpr para já.
data TinyL = TinyL Acsl [Stmt] Acsl Acsl 
           deriving Show

data Acsl = Old VarName RelBinOp IntExpr
          | Prop BoolExpr
          deriving Show

data Decl = Sdec VarName
          | Edec VarName IntExpr
          deriving Show

data Stmt = Attrib VarName IntExpr
          | If BoolExpr [Stmt]
          | TryCatch [Stmt] [Stmt]
          | Throw
          | IfThenElse BoolExpr [Stmt] [Stmt]
          | Loop BoolExpr Acsl [Stmt] {-Invariante como BoolExpr para já.-}
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



{-
data IntExpr = Val Int 
             | Var VarName
             | Add IntExpr IntExpr
             | Sub IntExpr IntExpr
             | Mul IntExpr IntExpr
             | Div IntExpr IntExpr
             deriving Show

data BoolExpr = Atom Int
              | Bvar VarName
              | Not BoolExpr
              | And BoolExpr BoolExpr
              | Or BoolExpr BoolExpr
              | GT BoolExpr BoolExpr
              | LT BoolExpr BoolExpr
              | GTE BoolExpr BoolExpr
              | LTE BoolExpr BoolExpr
              | EQ BoolExpr BoolExpr
              | Diff BoolExpr BoolExpr
              deriving Show

data TinyL = TinyL Pre [Decl] [Stmt] Pos Pos 
           deriving Show

data Pre = Old VarName
         | Nothing
         | BoolExpr
         deriving Show

data Inv = Nothing1 deriving Show

data Pos = Nothing2 deriving Show


-}