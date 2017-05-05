{-
Árvore de Sintaxe Abstrata da TinyLanguage a ser desenvolvida.
Falta a incorporação de Anotações
-}
type VarName = String

data TinyL = TinyL [Decl] [Stmt] deriving Show

data Decl = Sdec VarName
		  | Edec VarName IntExpr
		  deriving Show

data Stmt = Attrib VarName IntExpr
		  | If BoolExpr [Stmt]
		  | TryCatch [Stmt] [Stmt]
		  | Throw
		  | IfThenElse BoolExpr [Stmt] [Stmt]
		  | Loop BoolExpr [Stmt]
		  deriving Show

data IntExpr = Val Int 
			 | Var VarName
			 | Add IntExpr IntExpr
			 | Sub IntExpr IntExpr
			 | Mul IntExpr IntExpr
			 | Div IntExpr IntExpr
			 deriving Show

data BoolExpr = Atom Int
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
