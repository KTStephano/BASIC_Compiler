import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Sexpr = Symbol String | Number Int | Floating Double | Nil | Cons Sexpr Sexpr |
             Statement String Sexpr | Line Int Sexpr | Statements [Sexpr] |
             Expression String Sexpr | ExpressionList [Sexpr] | Variable Sexpr | 
             Array' String Sexpr | Function String Sexpr | Constant Sexpr | ID String | 
             Value Sexpr | IDList [Sexpr] | ArrayList [Sexpr] deriving (Eq)

instance Show Sexpr where
    show (Symbol x) = x
    show (Number x) = show x
    show (Floating x) = show x
    show (Statement s s') = "{Statement: " ++ s ++ " -> " ++ show s' ++ "}"
    show (Expression s e) = "{Expression: " ++ s ++ " -> " ++ show e ++ "}"
    show (Constant c) = show c
    show (Statements s) = show s
    show (ExpressionList es) = show es
    show (Variable s) = "{Var: " ++ show s ++ "}"
    show (Array' s s') = "{Array: " ++ s ++ " -> " ++ show s' ++ "}"
    show (Function s s') = "{Function: " ++ s ++ " -> " ++ show s' ++ "}"
    show (ID s) = "{ID: " ++ s ++ "}"
    show (Value s) = "{Value: " ++ show s ++ "}"
    show (Line i s) = show i ++ ": " ++ show s
    show (IDList s) = "{IDList: " ++ show s ++ "}"
    show (ArrayList s) = "{ArrayList: " ++ show s ++ "}"
    show Nil = "()"
    show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"

showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

string' = do
    char '"'
    s <- (many stringChars)
    symb "\""
    return ("\"" ++ s ++ "\"")

stringChars = alphanum +++ (sat isSpace) +++ misc

newline = do {symb "\n"} +++ do {symb "\r"}

iD = token letter

cdigit = do 
    c <- sat isDigit
    return c

integer' = token (do {s <- symb "-"; i <- many cdigit; return (read (s ++ i) :: Int)}) +++
           token (do {i <- many cdigit; return (read i :: Int)})

floating = (do {s <- symb "-"; i <- many cdigit; d <- char '.'; r <- many cdigit; return (read (s ++ i ++ [d] ++ r) :: Double)}) +++
           (do {i <- many cdigit; d <- char '.'; r <- many cdigit;  return (read (i ++ [d] ++ r) :: Double)})

line = do {symb "("; i <- integer'; s <- statements; symb ")"; return $ Line i s}

lines' = (do {l <- line; ls <- lines'; return $ Cons l ls}) +++ line

statements = (do {s <- statement; symb ":"; (Statements xs) <- statements; return $ Statements $ [s] ++ xs}) +++
             (do {s <- statement; return $ Statements [s]})

statement = dim +++ end +++ for +++ gotos +++ if' +++ input +++ let' +++ next +++ print' +++ return' +++ setVar

dim = do
    d <- symb "DIM"
    al <- arrayList
    return $ Statement d al

end = do
    d <- symb "END"
    return $ Statement d Nil

for = (do
    f <- symb "FOR"
    i <- iD
    symb "="
    ts <- to
    return $ Statement f (list (Symbol [i]) ts)) +++
    (do
        f <- symb "FOR"
        i <- iD
        symb "="
        ts <- to
        s <- step
        return $ Statement f $ Cons (Symbol [i]) (Cons ts (Cons s Nil)))

to = do
    e <- expression
    t <- symb "TO"
    e' <- expression
    return $ Statement t (list e e')

step = do
    s <- symb "STEP"
    e <- expression
    return $ Statement s e

-- Any function which operates on a single integer
gotos = do
    g <- (symb "GOTO" +++ symb "GOSUB")
    i <- integer'
    return $ Statement g (Number i)

then' = do
    t <- symb "THEN"
    i <- integer'
    return $ Statement t (Number i)

if' = do
    i <- symb "IF"
    e <- expression
    t <- then'
    return $ Statement i (list e t)

input = do
    i <- symb "INPUT"
    s <- string'
    ids <- idList
    return $ Statement i (list (Symbol s) ids)

next = do
    n <- symb "NEXT"
    ids <- idList
    return $ Statement n ids

return' = do
    r <- symb "RETURN"
    return $ Statement r Nil

let' = do
    l <- symb "LET"
    v <- variable
    symb "="
    e <- expression
    return $ Statement l (list v e)

-- for something like x = 1
setVar = Parser (\s -> parse let' ("LET" ++ s))

print' = do
    p <- symb "PRINT"
    el <- printList
    return $ Statement p el

list a b = Cons a (Cons b Nil)

expression = (do
    a <- andExp
    o <- symb "OR"
    e <- expression
    return $ Expression o $ Cons a e) +++ andExp

expressionList = (do {e <- expression; symb ","; (ExpressionList es) <- expressionList; return $ ExpressionList ([e] ++ es)}) +++
                 (do {e <- expression; return $ ExpressionList [e]})

idList = (do {i <- iD; (IDList ids) <- idList; return $ IDList ([ID [i]] ++ ids)}) +++
         (do {i <- iD; return $ IDList [ID [i]]})

printList = (do {e <- expression; (symb "," +++ symb ";"); (ExpressionList es) <- printList; return $ ExpressionList ([e] ++ es)}) +++
            (do {e <- expression; return $ ExpressionList [e]})

arrayList = (do {a <- array'; (ArrayList as) <- arrayList; return $ ArrayList ([a] ++ as)}) +++ 
            (do {a <- array'; return $ ArrayList [a]})

andExp = (do
    n <- notExp
    a <- symb "AND"
    n' <- notExp
    return $ Expression a $ Cons n n') +++ notExp

notExp = (do
    n <- symb "NOT"
    c <- compareExp
    return $ Expression n c) +++ compareExp

compareExp = equalExp +++ notEqualExp +++ greaterExp +++ gequalExp +++ lessExp +++ lequalExp +++ addExp

equalExp = do {a <- addExp; e <- symb "="; c <- compareExp; return $ Expression e $ list a c}

notEqualExp = do {a <- addExp; e <- symb "<>"; c <- compareExp; return $ Expression e $ list a c}

greaterExp = do {a <- addExp; e <- symb ">"; c <- compareExp; return $ Expression e $ list a c}

gequalExp = do {a <- addExp; e <- symb ">="; c <- compareExp; return $ Expression e $ list a c}

lessExp = do {a <- addExp; e <- symb "<"; c <- compareExp; return $ Expression e $ list a c}

lequalExp = do {a <- addExp; e <- symb "<="; c <- compareExp; return $ Expression e $ list a c}

addExp = (do {m <- multExp; s <- symb "+"; a <- addExp; return $ Expression s $ list m a}) +++
         (do {m <- multExp; s <- symb "-"; a <- addExp; return $ Expression s $ list m a}) +++
         multExp

multExp = (do {n <- negateExp; s <- symb "*"; m <- multExp; return $ Expression s $ list n m}) +++
          (do {n <- negateExp; s <- symb "/"; m <- multExp; return $ Expression s $ list n m}) +++
          negateExp

negateExp = (do {s <- symb "-"; p <- powerExp; return $ Expression s p}) +++ powerExp

powerExp = (do {v <- value; s <- symb "^"; p <- powerExp; return $ Expression s $ list v p}) +++
           value

value = (do {symb "("; e <- expression; symb ")"; return $ Value e}) +++
        (do {v <- variable; return $ Value v}) +++
        (do {f <- function; return $ Value f}) +++
        (do {c <- constant; return $ Value c})

variable = (do {i <- iD; return $ Variable $ ID [i]}) +++ (do {a <- array'; return $ Variable a})

array' = do {i <- iD; e <- expression; return $ Array' [i] $ ExpressionList [e]}

function = (do {s <- symb "INT"; symb "("; e <- expression; symb ")"; return $ Function s e}) +++
           (do {s <- symb "RND"; symb "("; e <- expression; symb ")"; return $ Function s e})

constant = (do {f <- floating; return $ Constant $ Floating f}) +++
           (do {i <- integer'; return $ Constant $ Number i}) +++
           (do {s <- string'; return $ Constant $ Symbol s})

misc = do
    r <- item--token item
    let miscVals = "<>^+-*/=!:."
    if (r `elem` miscVals) then return r else mzero

analyze str = let str' = map toUpper str
                  parsed = parse lines' str'
              in if (parsed == []) then Symbol "Error parsing input"
                 else fst $ parsed !! 0

