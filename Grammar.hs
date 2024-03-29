module Grammar where

import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Data.Functor.Identity
import Text.ParserCombinators.Parsec.Expr
import Control.Monad
import System.IO
import AST

reservedNames' = ["pre", "inv", "posn", "pose", "if", "then",
                    "else", "while", "true", "false",
                    "throw", "try", "catch", "old"]

reservedOps = ["+", "*", "/", "=", "==", "!=", "&&", "||",
                ">", "<", "<=", ">=", "!"]

tokens':: GenLanguageDef String st Identity
tokens' = LanguageDef {
    commentStart = "{-",
    commentEnd = "-}",
    commentLine = "--",
    nestedComments = True,
    identStart = letter,
    identLetter = alphaNum,
    opStart = oneOf "+*/=!&|<>",
    opLetter = oneOf "=&|",
    reservedNames = reservedNames',
    reservedOpNames = reservedOps,
    caseSensitive = True
}

lexer = P.makeTokenParser tokens'

identifier' = P.identifier lexer
reserved' = P.reserved lexer
reservedOp' = P.reservedOp lexer
operator' = P.operator lexer
integer' = P.integer lexer
whiteSpace' = P.whiteSpace lexer
parens' = P.parens lexer
braces' = P.braces lexer
semi' = P.semi lexer
semiSep' = P.semiSep lexer
semiSep1' = P.semiSep1 lexer
symbol' = P.symbol lexer

intOperators = [ [Prefix (reservedOp' "-" >> return (Neg))]
               , [Infix  (reservedOp' "*" >> return (IntBinary Mul)) AssocLeft,
                 Infix  (reservedOp' "/" >> return (IntBinary Div)) AssocLeft]
               , [Infix  (reservedOp' "+" >> return (IntBinary Add)) AssocLeft,
                 Infix  (reservedOp' "-" >> return (IntBinary Sub)) AssocLeft]
               ]

boolOperators = [ [Prefix (reservedOp' "!" >> return (Not))]
                , [Infix  (reservedOp' "&&" >> return (BoolBinary And)) AssocLeft,
                  Infix  (reservedOp' "||"  >> return (BoolBinary Or)) AssocLeft]
                ]

tinyL = do{
            whiteSpace';
            p <- pre;
            --d <- decls;
            s <- statments;
            p1 <- posn;
            p2 <- pose;

            return $ TinyL p s p1 p2
}


pre = do{
    reserved' "pre";
    a <- boolexp;
    semi';

    return a
}

statments = many statment

statment = atrib 
           <|>
           try condIf
           <|>
           condIfThenElse
           <|>
           trycatch
           <|>
           throw
           <|>
           loop

atrib = do {
    id <- identifier';
    reservedOp' "=";
    v <- intexp;
    semi';

    return $ Attrib id v
}

condIf = do{
    reserved' "if";
    b <- parens' boolexp;
    reserved' "then";
    s <- statments;

    return $ If b s
}

condIfThenElse = do{
    reserved' "if";
    b <- boolexp;
    reserved' "then";
    s1 <- statments;
    reserved' "else";
    s2 <- statments;

    return $ IfThenElse b s1 s2 
}

trycatch = do {
    reserved' "try";
    s1 <- braces' statments;
    reserved' "catch";
    s2 <- braces' statments;

    return $ TryCatch s1 s2
}

throw = do {
    reserved' "throw";
    semi';
    return Throw
}

loop = do{
    reserved' "while";
    b <- parens' boolexp;
    symbol' "{";
    l <- inv;
    s <- statments;
    symbol' "}";

    return $ Loop b l s
}

intexp = buildExpressionParser intOperators intTerm


boolexp = buildExpressionParser boolOperators boolTerm


intTerm = parens' intexp
        <|> liftM Var identifier'
        <|> liftM IntConst integer'

boolTerm = parens' boolexp
         <|> (reserved' "true" >> return (BoolConst True))
         <|> (reserved' "false" >> return (BoolConst False))
         <|> relexp

relexp = do{
    i1 <- intexp;
    op <- relation;
    i2 <- intexp;

    return $ RelationalBinary op i1 i2
}

relation = (reservedOp' ">" >> return AST.GT)
         <|> (reservedOp' "<" >> return AST.LT)
         <|> (reservedOp' ">=" >> return AST.GTE)
         <|> (reservedOp' "<=" >> return AST.LTE)
         <|> (reservedOp' "==" >> return AST.EQ)
         <|> (reservedOp' "!=" >> return AST.Diff)

posn = do{
    reserved' "posn";
    a <- boolexp;
    semi';

    return a
}

pose = do{
    reserved' "pose";
    a <- boolexp;
    semi';

    return a
}

inv = do{
    reserved' "inv";
    a <- boolexp;
    semi';

    return a
}

parseFile :: String -> IO TinyL
parseFile f = do {
    lang <- readFile f;
        case parse tinyL "" lang of 
            Left e -> print e >> fail "Parse Error"
            Right r -> return r
}