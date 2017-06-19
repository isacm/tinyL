module VCGen where

import TinyL
import Z3.Monad

{-
    Perguntar:
        -> Como faço a substituição Q[x/e] ??
            « wp [Attrib nome valor] posCond
        
        Pela Def. dos slides:
            « wp :: [Stmt] -> AST -> z3 AST
        
        Se wp :: [Stmt] -> Acsl -> z3 AST era fácil:
            « subACSL :: Acsl -> String -> IntExpr -> Acsl

        -> Como definir Old v op iexp ??

        -> intExprToZ3 (Var s) está bem ??
            « intExprToZ3 (Var s) = mkFreshIntVar s
-}

--acslToZ3 Old v op iexp = undefined

acslToZ3 (Prop bexp) = boolExprToZ3 bexp

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
}

boolExprToZ3 (RelationalBinary op exp1 exp2) = do {
    zexp1 <- intExprToZ3 exp1;
    zexp2 <- intExprToZ3 exp2;

    case op of
        TinyL.GT -> mkGt zexp1 zexp2
        TinyL.LT -> mkLt zexp1 zexp2
        TinyL.GTE -> mkGe zexp1 zexp2
        TinyL.LTE -> mkLe zexp1 zexp2
        TinyL.EQ -> mkEq zexp1 zexp2
        TinyL.Diff -> mkEq zexp1 zexp2 >>= mkNot
}

--vcg:: TinyL -> z3 AST
vcg (TinyL p s q1 q2) = do {
    sort <- mkBoolSort;
    set <- mkEmptySet sort;
    p' <- acslToZ3 p;
    q1' <- acslToZ3 q1;
    q' <- wp s q1';
    imp <- mkImplies p' q';
    s' <- mkSetAdd set imp;
    s2 <- vcaux s q1';
    
    mkSetUnion [s', s2]
}

--vcaux:: [Stmt] -> Acsl -> z3 AST -- não pode ser!
--vcaux:: [Stmt] -> AST -> z3 AST
vcaux [] q = mkBoolSort >>= mkEmptySet

vcaux [Attrib v i] q = mkBoolSort >>= mkEmptySet

vcaux [If b s] q = vcaux s q

vcaux [IfThenElse b s1 s2] q = do {
    v1 <- vcaux s1 q;
    v2 <- vcaux s2 q;

    mkSetUnion [v1, v2]
}

vcaux [Loop b i s] q = do {
    b' <- boolExprToZ3 b;
    i' <- acslToZ3 i;
    --q' <- acslToZ3 q;
    nb' <- mkNot b';
    w <- wp s i';
    sort <- mkBoolSort;
    set <- mkEmptySet sort;

    bi <- mkAnd [b', i'];
    nbi <- mkAnd [nb', i'];
    imp1 <- mkImplies bi w;
    imp2 <- mkImplies nbi q;

    set' <- mkSetAdd set imp1;
    set''<- mkSetAdd set' imp2;

    vcs <- vcaux s i'; -- para i ser AST

    mkSetUnion [set'', vcs]
}

vcaux (s1:sn) q = do {
    w <- wp sn q;
    v1 <- vcaux [s1] w;
    v2 <- vcaux sn q;
    sort <- mkBoolSort;
    set1 <- mkEmptySet sort;
    set2 <- mkEmptySet sort;
    r1 <- mkSetAdd set1 v1;
    r2 <- mkSetAdd set2 v2;

    mkSetUnion [r1, r2]
}

--wp:: [Stmt] -> Acsl -> z3 AST -- não pode ser!
--wp:: [Stmt] -> AST -> z3 AST
wp [] q = return q  --acslToZ3 q

{-
wp [Attrib v i] q = do
    let q' = subACSL q v i --quero o teu var de i
    acslToZ3 q'
-}

wp [If b s] q = do {
    b' <- boolExprToZ3 b;
    w <- wp s q;
    mkImplies b' w
}

wp [IfThenElse b s1 s2] q = do {
    b' <- boolExprToZ3 b;
    nb <- mkNot b';
    w1 <- wp s1 q;
    w2 <- wp s2 q;
    imp1 <- mkImplies b' w1;
    imp2 <- mkImplies nb w2;

    mkAnd [imp1, imp2]

}

wp [Loop b i s] q = acslToZ3 i

wp (s1:sn) q = do {
    w <- wp sn q;
    wp [s1] w
}


subACSL :: Acsl -> String -> IntExpr -> Acsl
subACSL (Prop bexp) x e = Prop (subBexp bexp x e)
subACSL exp x e = exp

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