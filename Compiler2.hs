module Compiler2
(Sexpr(Symbol, Number, Floating, Nil, Cons), 
 Value(VIntegral, VFloating, VString, VSymbol, VBool, VStatement, Null), 
 Bytecode(End, Push, Print, PrintBang, Add, Mult, Sub, Div, Load, Store, Input, Equal, NotEqual, Greater, GEqual, Less, LEqual, IfThen, Goto), 
 analyze, car, cdr, printBytecode) where

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe
import Control.Monad.State
import Control.Monad.Trans.State

foo = "(define foo " ++
      "'((100 input \"What is the value of A\" a )" ++
      " (110 input \"What is the value of B\" b )" ++
      " (120 input \"What is the value of C\" c )" ++
      " (130 let d = ((b * b) - (4.0 * (a * c))) )" ++
      " (140 print d) (150 end)))"

quadratic1 = "(define quadratic1 '(" ++
    "(100 input \"What is the value of A\" a )" ++
    "(110 input \"What is the value of B\" b )" ++
    "(120 input \"What is the value of C\" c )" ++
    "(130 let d = ((b * b) - (4.0 * (a * c))) )" ++
    "(140 if (d < 0) then 230 )" ++ 
    "(150 let i = 0 )" ++ 
    "(160 let s = 1 )" ++
    "(170 let s = ((s + (d / s)) / 2.0) )" ++
    "(180 let i = (i + 1) )" ++
    "(190 if (i < 10) then 170 )" ++
    "(200 print \"The 1st root is: \" (((-1.0 * b) + s) / (2.0 * a)) )" ++ 
    "(210 print \"The 2nd root is: \" (((-1.0 * b) - s) / (2.0 * a)) )" ++ 
    "(220 end )" ++
    "(230 print \"Imaginary roots.\" )" ++
    "(240 end )))"

quadratic2 = "(define quadratic2 '(" ++
    "(100 input \"What is the value of A\" a )" ++
    "(110 input \"What is the value of B\" b )" ++
    "(120 input \"What is the value of C\" c )" ++
    "(130 let d = ((b * b) - (4.0 * (a * c))) )" ++
    "(140 if (d < 0) then 190 )" ++
    "(150 gosub 210 )" ++
    "(160 print \"The 1st root is: \" (((-1.0 * b) + s) / (2.0 * a)) )" ++
    "(170 print \"The 2nd root is: \" (((-1.0 * b) - s) / (2.0 * a)) )" ++
    "(180 end )" ++
    "(190 print \"Imaginary roots.\" )" ++
    "(200 end )" ++
    "(210 let s = 1 )" ++
    "(220 for i = 1 to 10 )" ++
    "(230 let s = ((s + (d / s)) / 2.0) )" ++
    "(240 next i )" ++
    "(250 return )" ++
    "(260 i = 2)))"

data Sexpr = Symbol String | Number Int | Floating Double | Nil | Cons Sexpr Sexpr deriving (Eq)

car (Cons a b) = a
cdr (Cons a b) = b

instance Show Sexpr where
    show (Symbol x) = x
    show (Number x) = show x
    show (Floating x) = show x
    show Nil = "()"
    show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"

showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

data Bytecode = End {line :: Int} | Push {line :: Int, arg :: Value} | Print {line :: Int} | PrintBang {line :: Int} | 
                Add {line :: Int} | Mult {line :: Int} | Sub {line :: Int} | Div {line :: Int} | Load {line :: Int} | Store {line :: Int} |
                Input {line :: Int} | Equal {line :: Int} | NotEqual {line :: Int} | Greater {line :: Int} | 
                GEqual {line :: Int} | Less {line :: Int} | LEqual {line :: Int} | IfThen {line :: Int} | 
                Goto {line :: Int} | NextLine {line :: Int} | PushCallstack {line :: Int} |
                PopCallstack {line :: Int} deriving (Show, Eq)

data Value = VIntegral Int | VFloating Double | VString String | VSymbol {name :: String, val :: Value} |
             VBool Bool | VStatement [Bytecode] | Null | VPair (Value, Value) deriving (Eq)

instance Show Value where
    show (VIntegral x) = show x
    show (VFloating d) = show d
    show (VString s) = s
    show (VBool b) = show b
    show Null = "Null"
    show (VSymbol vr vl) = vr ++ "," ++ show vl
    show (VStatement s) = show s

{-

{symbol} ::= {first} {symbolic}* | {string}
{first} ::= {misc} | {lower}
{symbolic} ::= {first} | {digit}
{misc} ::= ’<’ | ’>’ | ’ˆ’ | ’+’ | ’-’ | ’*’ | ’/’ | ’=’
{integernum} ::= {digit}+
{number} ::= {digit}+ . {digit}+
S ::= () | (E) | '(E) | A | (S . S)
E ::= (E)E | S E | S                --> This returns a pair of sexprs
A ::= {symbol} | {number} | {integernum}

-}

quote = do
    x <- char '"'
    y <- (many quotedString)
    z <- symb "\""
    return ([x] ++ y ++ z)

quotedString = alphanum +++ (sat isSpace) +++ misc

cdigit = do 
    c <- sat isDigit
    return c

integernum = (do
    r <- many1 cdigit
    return (read r :: Int)) +++
    (do
        s <- char '-'
        r <- many1 cdigit
        return (read ([s] ++ r) :: Int))

number = (do
    r <- many1 cdigit
    d <- symb "."
    l <- many1 cdigit
    return (read (r ++ d ++ l) :: Double)) +++
    (do
        s <- char '-'
        r <- many1 cdigit
        d <- symb "."
        l <- many1 cdigit
        return (read ([s] ++ r ++ d ++ l) :: Double))

misc = do
    r <- item--token item
    let miscVals = ['<', '>', '^', '+', '-', '*', '/', '=', '!', ':', '.']
    if (r `elem` miscVals) then return r else mzero

first = misc +++ lower

symbolic = first +++ cdigit --(token first) +++ (token cdigit)

symbol = (do
    f <- first
    s <- token (many symbolic)
    return (f:s)) +++ quote

nil = do
    symb "("
    symb ")"
    return Nil

wrappedE = do
    (symb "(" +++ symb "'(")
    res <- token e
    symb ")"
    return res

ss = do
    symb "("
    left <- token s
    symb "."
    right <- token s
    symb ")"
    return $ Cons left right
    
ee = do
    res <- wrappedE
    recur <- token e
    return $ Cons res recur

se = do
    left <- token s
    right <- token e
    return $ Cons left right

s = nil +++ wrappedE +++ a +++ ss

e = ee +++ se +++ (do {res <- s; return $ Cons res Nil})

--A :: Parser Sexpr
a :: Parser Sexpr
a =     (do
            n <- number
            return $ Floating n) +++
            (do
                n <- integernum
                return $ Number n) +++
                (do
                    s <- symbol
                    return $ Symbol s)

p str = let result = parse s str
        in if (result == []) then Symbol "Error parsing string"
           else fst (result !! 0)

-- Parses the string and returns it in a form that is compiler-friendly
analyze :: [Char] -> Sexpr
analyze str = let result = p str
              in case result of 
                (Symbol s) -> Symbol s -- Parser returned an error
                s -> car $ cdr $ cdr s

printBytecode :: [Bytecode] -> IO ()
printBytecode [] = putStr ""
printBytecode (x:xs) = do
    putStrLn (show x)
    printBytecode xs

data Basic = String' String | Integer' Int | Floating' Double | ID String | StatementList [Basic] |
             Line Int Basic | Lines [Basic] | Variable Basic | Function String Basic | Value Basic | 
             Statement {getName :: String, body :: Basic} | 
             Expression String Basic | ExpressionList [Basic] | None deriving (Eq)

instance Show Basic where
    show (String' s) = s
    show (Integer' i) = show i
    show (Floating' f) = show f
    show (ID s) = "ID " ++ s
    show (StatementList xs) = show xs
    show (Line l b) = show l ++ ": " ++ show b
    show (Variable b) = "Var " ++ show b
    show (Function s b) = "Function " ++ s ++ " -> " ++ show b
    show (Statement s b) = "{Statement " ++ show s ++ " -> " ++ show b ++ "}"
    show (Expression s b) = "{Expression " ++ show s ++ " -> " ++ show b ++ "}"
    show (ExpressionList xs) = show xs
    show None = "None"

type ParseTree a = StateT Sexpr Maybe a

item' :: ParseTree Sexpr
item' = StateT $ \s -> if s == Nil then Nothing else 
    case (car s) of
        Nil -> Nothing
        s' -> Just (s', cdr s)

string' cs = do {i <- item'; if (Symbol cs) == i then return i else mzero}

symb' :: Sexpr -> ParseTree Sexpr
symb' cs = do {i <- item'; if cs == i then return i else mzero}

-- Functions for converting a Sexpr (parse tree) into Basic
iD = do
    (Symbol s) <- item'
    if (s !! 0) == '"' then mzero
    else return $ ID s

basicString =
    do
        (Symbol s@('"':xs)) <- item'
        return $ String' s

statements :: ParseTree Basic
statements =
    (do
        s <- statement
        string' ":"
        (StatementList ss) <- statements
        return $ StatementList ([s] ++ ss)) `mplus`
    (do
        s <- statement
        return $ StatementList [s])

statement :: ParseTree Basic
statement = end `mplus` goto `mplus` if' `mplus` input `mplus` let' `mplus` print' `mplus` return' `mplus` setVar

end :: ParseTree Basic
end = do
    (Symbol e) <- string' "end"
    return $ Statement e None

goto :: ParseTree Basic
goto = do
    (Symbol e) <- (string' "goto" `mplus` string' "gosub")
    (Number i) <- item'
    return $ Statement e $ Integer' i

if' :: ParseTree Basic
if' = do
    (Symbol i) <- string' "if"
    e <- expression
    t <- then'
    return $ Statement i $ ExpressionList [e, t]

then' :: ParseTree Basic
then' = do
    (Symbol t) <- string' "then"
    e <- expression
    return $ Statement t e

input :: ParseTree Basic
input = do
    (Symbol i) <- string' "input"
    (Symbol s) <- item'
    var <- iD
    return $ Statement i $ ExpressionList [String' s, var]

let' :: ParseTree Basic
let' = do
    (Symbol l) <- string' "let"
    v <- variable
    string' "="
    e <- expression
    return $ Statement l $ ExpressionList [v, e]

print' :: ParseTree Basic
print' = (do
    (Symbol p) <- string' "print"
    es <- expressionList
    return $ Statement p es) `mplus`
    (do
        (Symbol p) <- string' "print"
        t <- tab
        (ExpressionList es) <- expressionList
        return $ Statement p $ ExpressionList ([t] ++ es))

return' :: ParseTree Basic
return' = do
    (Symbol r) <- string' "return"
    return $ Expression r None


tab :: ParseTree Basic
tab = do
    (Symbol t) <- string' "tab"
    e <- expression
    return $ Statement t e

setVar :: ParseTree Basic
setVar = do
    v <- variable
    string' "="
    e <- expression
    return $ Statement "let" $ ExpressionList [v, e]

optimization = StateT $ \s -> if s == Nil then Nothing else
    case s of
        (Cons c@(Cons s s') s'') ->
            case (runStateT expression c) of
                Nothing -> Nothing
                Just (res, _) -> Just (res, s'')
        _ -> Nothing

expression :: ParseTree Basic
expression = (do
        a <- andExp
        (Symbol o) <- string' "or"
        e <- expression
        return $ Expression o $ ExpressionList [a, e]) `mplus` andExp

expressionList :: ParseTree Basic
--expressionList = (do {e <- expression; (ExpressionList es) <- expressionList; return $ ExpressionList ([e] ++ es)}) `mplus`
                 --(do {e <- expression; return $ ExpressionList [e]})

expressionList =
    (do
        e <- expression
        (ExpressionList es) <- expressionList
        return $ ExpressionList ([e] ++ es)) `mplus`
    (do
        e <- expression
        return $ ExpressionList [e])

andExp =
    (do
        n <- notExp
        (Symbol a) <- string' "and"
        ae <- andExp
        return $ Expression a $ ExpressionList [n, ae]) `mplus` notExp

notExp =
    (do
        (Symbol n) <- string' "not"
        c <- compareExp
        return $ Expression n c) `mplus` compareExp

compareExp = (do {a <- addExp; (Symbol equ) <- string' "="; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             (do {a <- addExp; (Symbol equ) <- string' "<>"; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             (do {a <- addExp; (Symbol equ) <- string' ">"; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             (do {a <- addExp; (Symbol equ) <- string' ">="; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             (do {a <- addExp; (Symbol equ) <- string' "<"; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             (do {a <- addExp; (Symbol equ) <- string' "<="; c <- compareExp; return $ Expression equ $ ExpressionList [a, c]}) `mplus`
             addExp

addExp :: ParseTree Basic
addExp = (do {m <- multExp; (Symbol op) <- string' "+"; a <- addExp; return $ Expression op $ ExpressionList [m, a]}) `mplus`
         (do {m <- multExp; (Symbol op) <- string' "-"; a <- addExp; return $ Expression op $ ExpressionList [m, a]}) `mplus`
         multExp

multExp :: ParseTree Basic
multExp = (do {n <- negateExp; (Symbol op) <- string' "*"; m <- multExp; return $ Expression op $ ExpressionList [n, m]}) `mplus`
          (do {n <- negateExp; (Symbol op) <- string' "/"; m <- multExp; return $ Expression op $ ExpressionList [n, m]}) `mplus`
          negateExp

negateExp :: ParseTree Basic
negateExp = (do {(Symbol op) <- string' "-"; p <- powerExp; return $ Expression op $ ExpressionList [p]}) `mplus`
            powerExp

powerExp = (do {v <- value; (Symbol op) <- string' "^"; p <- powerExp; return $ Expression op $ ExpressionList [v, p]}) `mplus`
           value

value = variable `mplus` function `mplus` constant `mplus` optimization

variable = iD

function = (do {(Symbol i) <- string' "int"; e <- expression; return $ Function i e})

constant = (do {(Number i) <- item'; return $ Integer' i}) `mplus`
           (do {(Floating f) <- item'; return $ Floating' f}) `mplus`
           basicString

--renumber :: [Basic] -> ([Basic], [(Int, Int)])
{-
renumber [] _ _ mapping = ([], mapping)
renumber basic@((Statement l n b):xs) line newline mapping = if l /= line then renumber basic l (newline + 1) mapping
                                                             else let s = [Statement newline n b]
                                                                      (ys, mapping') = renumber xs line newline mapping
                                                                  in if ((l, newline) `elem` mapping') then (s, mapping')
                                                                     else (s ++ ys, [(l, newline)] ++ mapping')
-}

translateSexpr Nil = []
translateSexpr (Cons (Number i) s) = let result = runStateT statements s
                                     in case result of
                                        (Just (r@(StatementList ss), _)) -> [Line i r]
                                        Nothing -> []
translateSexpr (Cons s s') = translateSexpr s ++ translateSexpr s'