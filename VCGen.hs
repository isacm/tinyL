module VCGen where

import AST
import Z3.Monad
import qualified Data.Set as Set

vcs :: MonadZ3 z3 => TinyL -> z3 [AST]
vcs = sequence . negVcs . vcg2Z3 . vcg
--vcs = sequence . vcg2Z3 . negBoolExp . vcg


vcs' :: MonadZ3 z3 => TinyL -> z3 AST
vcs' t = vcs t >>= mkAnd

vcg2Z3 ::MonadZ3 z3 => Set.Set BoolExpr -> [z3 AST]
vcg2Z3 s | Set.null s = []
         | otherwise = map boolExprToZ3 (Set.toList s)

negBoolExp exps = Set.map (\b -> (Not b)) exps 

negVcs :: MonadZ3 z3 => [z3 AST] -> [z3 AST]
negVcs l = map (\x -> x >>= mkNot) l

intExprToZ3 (Var s) = mkFreshIntVar s

intExprToZ3 (IntConst i) = mkInteger i

intExprToZ3 (Neg exp) = intExprToZ3 exp >>= mkUnaryMinus

intExprToZ3 (IntBinary op exp1 exp2) = do {
    zexp1 <- intExprToZ3 exp1;
    zexp2 <- intExprToZ3 exp2; 
    case op of
        Add -> mkAdd [zexp1, zexp2]
        Sub -> mkSub [zexp1, zexp2]
        Mul -> mkMul [zexp1, zexp2]
        Div -> mkDiv zexp1 zexp2
}

boolExprToZ3 (BoolConst b) | b = mkTrue
                         | otherwise = mkFalse

boolExprToZ3 (Not exp) = boolExprToZ3 exp >>= mkNot

boolExprToZ3 (BoolBinary op exp1 exp2) = do {
    zexp1 <- boolExprToZ3 exp1;
    zexp2 <- boolExprToZ3 exp2;

    case op of
        And -> mkAnd [zexp1, zexp2]
        Or -> mkOr [zexp1, zexp2]
        _ -> mkImplies zexp1 zexp2
}

boolExprToZ3 (RelationalBinary op exp1 exp2) = do {
    zexp1 <- intExprToZ3 exp1;
    zexp2 <- intExprToZ3 exp2;

    case op of
        AST.GT -> mkGt zexp1 zexp2
        AST.LT -> mkLt zexp1 zexp2
        AST.GTE -> mkGe zexp1 zexp2
        AST.LTE -> mkLe zexp1 zexp2
        AST.EQ -> mkEq zexp1 zexp2
        AST.Diff -> mkEq zexp1 zexp2 >>= mkNot
}

vcg :: TinyL -> Set.Set BoolExpr
vcg (TinyL p s q1 q2) = Set.union  (Set.singleton (BoolBinary Implies p (wp s q1 q2))) (vcaux s q1 q2)

vcaux :: [Stmt] -> BoolExpr -> BoolExpr -> Set.Set BoolExpr
vcaux [] q1 q2 = Set.empty
vcaux [Throw] q1 q2 = Set.empty
vcaux [Attrib v i] q1 q2 = Set.empty
vcaux [TryCatch s1 s2] q1 q2 = Set.union (vcaux s1 q1 q2) (vcaux s2 q1 q2)
vcaux [If b s] q1 q2 = vcaux s q1 q2
vcaux [IfThenElse b s1 s2] q1 q2 = Set.union (vcaux s1 q1 q2) (vcaux s2 q1 q2)
vcaux [Loop b i s] q1 q2 = Set.union (Set.fromList ([BoolBinary Implies (BoolBinary And i b) (wp s i q2),
                            BoolBinary Implies (BoolBinary And i (Not b)) q1])) (vcaux s i q2)

vcaux (s1:sn) q1 q2 = Set.union (vcaux [s1] (wp sn q1 q2) q2) (vcaux sn q1 q2)

wp:: [Stmt] -> BoolExpr -> BoolExpr -> BoolExpr
wp [] q1 q2 = q1
wp [Throw] q1 q2 = q2
wp [Attrib x e] q1 q2 = subBexp q1 x e
wp [TryCatch s1 s2] q1 q2 = (wp s1 q1 (wp s2 q1 q2)) 
wp [If b s] q1 q2 = BoolBinary Implies b (wp s q1 q2)
wp [IfThenElse b s1 s2] q1 q2 = BoolBinary And (BoolBinary Implies b (wp s1 q1 q2)) (BoolBinary Implies (Not b) (wp s2 q1 q2))
wp [Loop b i s] q1 q2 = i
wp (s1:sn) q1 q2 = (wp [s1] (wp sn q1 q2) q2)

subBexp :: BoolExpr -> String -> IntExpr -> BoolExpr
subBexp (RelationalBinary op exp1 exp2) x e = RelationalBinary op (substitute exp1 x e) (substitute exp2 x e)
subBexp (Not exp) x e = Not (subBexp exp x e)
subBexp (BoolBinary op exp1 exp2) x e = BoolBinary op (subBexp exp1 x e) (subBexp exp2 x e) 
subBexp exp x e = exp

substitute:: IntExpr -> String -> IntExpr -> IntExpr
substitute (Var s) x e = if s == x then e
                            else Var s

substitute (Neg exp) x e = Neg (substitute exp x e)
substitute (IntBinary op exp1 exp2) x e = IntBinary op (substitute exp1 x e) (substitute exp2 x e)
substitute exp x e = exp