module Compiler
(Sexpr(Symbol, Number, Floating, Nil, Cons), 
 Value(VIntegral, VFloating, VString, VSymbol, VBool, VStatement, Null), 
 Bytecode(End, Push, Print, PrintBang, Add, Mult, Sub, Div, Load, Store, Input, Equal, NotEqual, Greater, GEqual, Less, LEqual, IfThen, Goto, PushCallstack, PopCallstack, NextLine, Spaces, CastInt, Rand, Log, Abs, Pow), 
 analyze, car, cdr, compile, printBytecode, line) where

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

guess = "(define guess '( (100 print tab(33) \"GUESS\" ) (110 print tab(15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (120 print ) (130 print \"THIS IS A NUMBER GUESSING GAME. I'LL THINK\" ) (140 print \"OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.\" ) (150 print \"THEN YOU HAVE TO GUESS WHAT IT IS.\" ) (160 print ) (170 input \"WHAT LIMIT DO YOU WANT\" l ) (180 print ) (190 let l1 = int (((log (l) / log (2)) + 1)) ) (200 print \"I'M THINKING OF A NUMBER BETWEEN 1 AND \" l ) (210 let g = 1 ) (220 let m = int (((l * rnd (1)) + 1)) ) (230 print ) (240 input \"WHAT IS YOUR GUESS\" n ) (250 print ) (260 if (n > 0) then 290 ) (270 print \"ILLEGAL VALUE.\" ) (280 goto 230 ) (290 if (n <= l) then 320 ) (300 print \"ILLEGAL VALUE.\" ) (310 goto 230 ) (320 if (n = m) then 390 ) (330 let g = (g + 1) ) (340 if (n > m) then 370 ) (350 print \"TOO LOW. TRY A BIGGER ANSWER.\" ) (360 goto 230 ) (370 print \"TOO HIGH. TRY A SMALLER ANSWER.\" ) (380 goto 230 ) (390 print \"THAT'S IT! YOU GOT IT IN \" g \" TRIES.\" ) (400 if (g < l1) then 440 ) (410 if (g = l1) then 450 ) (420 print \"YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY \" l1 \" TRIES.\" ) (430 end ) (440 print! \"VERY \" ) (450 print \"GOOD.\" ) (460 end )))"

hamurabi = "(define hamurabi '((1000 print \"HAMURABI: Game of Hamurabi - Version 1.01\" )(1010 print )(1020 print \"Corona Data Systems, Inc.\" )(1030 print )(1040 print \"HAMURABI - \" )(1050 print \"WHERE YOU GOVERN THE ANCIENT KINGDOM OF SUMERIA.\" )(1060 print \"THE OBJECT IS TO FIGURE OUT HOW THE GAME WORKS!!\" )(1070 print \"IF YOU WANT TO QUIT, SELL ALL YOUR LAND.\" )(1080 let a1 = 100 )(1090 let a2 = 5 )(1100 let a3 = 0 )(1110 let b1 = 2800 )(1120 let b2 = 200 )(1130 let b3 = 3 )(1140 let b4 = 3000 )(1150 let c1 = 1000 )(1160 let j = 1 )(1170 print )(1180 print \"HAMURABI, I BEG TO REPORT THAT LAST YEAR \" a3 \" PEOPLE\" )(1190 print \"STARVED AND \" a2 \" PEOPLE CAME TO THE CITY.\" )(1200 if (j > 0) then 1230 )(1210 let a1 = (a1 - int ((a1 / 2))) )(1220 print \"THE PLAGUE KILLED HALF THE PEOPLE.\" )(1230 print \"THE POPULATION IS NOW \" a1 \".\" )(1240 print \"WE HARVESTED \" b4 \" BUSHELS AT \" b3 \" BUSHELS PER ACRE.\" )(1250 print \"RATS DESTROYED \" b2 \" BUSHELS, LEAVING \" b1 \" BUSHELS\" )(1260 print \"IN THE STOREHOUSES.\" )(1270 print \"THE CITY OWNS \" c1 \" ACRES OF LAND.\" )(1280 let c2 = (17 + int ((6 * rnd (1)))) )(1290 print \"LAND IS WORTH \" c2 \" BUSHELS PER ACRE.\" )(1300 print )(1310 print \"HAMURABI...\" )(1320 print )(1330 input \"HOW MANY ACRES DO YOU WISH TO BUY\" i )(1340 print )(1350 let i = int (abs (i)) )(1360 if (i = 0) then 1430 )(1370 let j = (i * c2) )(1380 if (j <= b1) then 1410 )(1390 gosub 1850 )(1400 goto 1330 )(1410 let b1 = (b1 - j) )(1420 let c1 = (c1 + i) )(1430 input \"HOW MANY ACRES DO YOU WISH TO SELL\" i )(1440 print )(1450 let i = abs (i) )(1460 if (i = 0) then 1530 )(1470 if (i < c1) then 1510 )(1480 if (i = c1) then end )(1490 gosub 1850 )(1500 goto 1430 )(1510 let c1 = (c1 - i) )(1520 let b1 = (b1 + (c2 * i)) )(1530 input \"HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD\" i )(1540 print )(1550 let i = int (abs (i)) )(1560 if (i <= b1) then 1590 )(1570 gosub 1850 )(1580 goto 1530 )(1590 let b1 = (b1 - i) )(1600 let a3 = (a1 - int ((i / 20))) )(1610 let a2 = 0 )(1620 if (a3 >= 0) then 1650 )(1630 let a2 = ((-1 * a3) / 2) )(1640 let a3 = 0 )(1650 input \"HOW MANY ACRES SHALL WE PLANT\" i )(1660 print )(1670 let i = int (abs (i)) )(1680 if (i > c1) then 1710 )(1690 let j = int ((i / 2)) )(1700 if (j <= b1) then 1730 )(1710 gosub 1850 )(1720 goto 1650 )(1730 if (i > (10 * a1)) then 1710 )(1740 let b1 = (b1 - j) )(1750 let b3 = (int ((5 * rnd (1))) + 1) )(1760 let b4 = (b3 * i) )(1770 let b2 = int ((((b1 + b4) * 0.07) * rnd (1))) )(1780 let b1 = ((b1 - b2) + b4) )(1790 let j = int ((10 * rnd (1))) )(1800 let a2 = int ((a2 + ((((5 - b3) * b1) / 600) + 1))) )(1810 if (a2 <= 50) then 1830 )(1820 let a2 = 50 )(1830 let a1 = (a1 + (a2 - a3)) )(1840 goto 1170 )(1850 print \"HAMURABI, THINK AGAIN - \" )(1860 print \"YOU ONLY HAVE \" a1 \" PEOPLE, \" c1 \" ACRES, AND \" )(1870 print b1 \" BUSHELS IN STOREHOUSES.\" )(1880 return )))"

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
                Add {line :: Int} | Mult {line :: Int} | Sub {line :: Int} | Div {line :: Int} | Pow {line :: Int} |
                Load {line :: Int} | Store {line :: Int} | Input {line :: Int} | Equal {line :: Int} | 
                NotEqual {line :: Int} | Greater {line :: Int} | GEqual {line :: Int} | Less {line :: Int} | 
                LEqual {line :: Int} | IfThen {line :: Int} | Goto {line :: Int} | NextLine {line :: Int} | 
                PushCallstack {line :: Int} | PopCallstack {line :: Int} | Spaces {line :: Int} | CastInt {line :: Int} |
                Rand {line :: Int} | Log {line :: Int} | Abs {line :: Int} deriving (Show, Eq)

data Value = VIntegral Int | VFloating Double | VString String | VSymbol {name :: String, val :: Value} |
             VBool Bool | VStatement [Bytecode] | Null | VPair (Value, Value) deriving (Eq)

instance Show Value where
    show (VIntegral x) = show x
    show (VFloating d) = show d
    show (VString s) = s
    show (VBool b) = show b
    show Null = "Null"
    show (VSymbol vr vl) = show vl
    show (VStatement s) = show s

{-

{symbol} ::= {first} {symbolic}* | {string}
{first} ::= {misc} | {lower}
{symbolic} ::= {first} | {digit}
{misc} ::= '<' | '>' | '^' | '+' | '-' | '*' | '/' | '='
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
    let miscVals = ['<', '>', '^', '+', '-', '*', '/', '=', '!', ':', '.', '\'', ',', '$']
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
    putStr (show $ line x)
    putStr ": "
    putStrLn (show x)
    printBytecode xs

data Basic = String' String | Integer' Int | Floating' Double | StatementList [Basic] |
             Line Int Basic | Lines [Basic] | Variable Basic | Function String Basic | Value Basic | 
             Constant Basic | Statement {getName :: String, body :: Basic} | 
             Expression String Basic | ExpressionList [Basic] | 
             Array' String Int Basic | None deriving (Eq)

instance Show Basic where
    show (String' s) = s
    show (Integer' i) = show i
    show (Floating' f) = show f
    show (StatementList xs) = show xs
    show (Line l b) = show l ++ ": " ++ show b
    show (Variable b) = "Var " ++ show b
    show (Function s b) = "Function " ++ s ++ " -> " ++ show b
    show (Array' n i b) = "Array -> " ++ n ++ " " ++ show i ++ " " ++ show b
    show (Statement s b) = "{Statement " ++ s ++ " -> " ++ show b ++ "}"
    show (Expression s b) = "{Expression " ++ s ++ " -> " ++ show b ++ "}"
    show (ExpressionList xs) = show xs
    show (Constant c) = "Const " ++ show c
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
    (Symbol (s:ss)) <- item'
    if s == '"' then mzero
    else return $ Variable $ String' (s:ss)

basicString =
    do
        (Symbol s@('"':xs)) <- item'
        return $ Constant $ String' s

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
statement = dim `mplus` end `mplus` for `mplus` goto `mplus` if' `mplus` input `mplus` let' `mplus` next `mplus` print' `mplus` return' `mplus` setVar

dim :: ParseTree Basic
dim = do
    (Symbol d) <- string' "dim"
    a <- array'
    return $ Statement d a

end :: ParseTree Basic
end = do
    (Symbol e) <- string' "end"
    return $ Statement e None

for :: ParseTree Basic
for = do
    (Symbol f) <- string' "for"
    i <- iD
    string' "="
    ts@(Statement t (ExpressionList es)) <- to
    return $ Statement f $ ExpressionList [i, (Statement t $ ExpressionList ([i] ++ es))]

to :: ParseTree Basic
to = do
    e <- expression
    (Symbol t) <- string' "to"
    e' <- expression
    return $ Statement t $ ExpressionList [e, e']

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
    e <- (nesting statement `mplus` statement)
    return $ Statement t e

input :: ParseTree Basic
input = do
    (Symbol i) <- string' "input"
    s <- basicString
    var <- iD
    return $ Statement i $ ExpressionList [s, var]

let' :: ParseTree Basic
let' = do
    (Symbol l) <- string' "let"
    v <- variable
    string' "="
    e <- expression
    return $ Statement l $ ExpressionList [v, e]

next :: ParseTree Basic
next = do
    (Symbol n) <- string' "next"
    i <- iD
    return $ Statement n i

print' :: ParseTree Basic
print' = 
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        t <- tab
        (ExpressionList es) <- expressionList
        return $ Statement p $ ExpressionList ([t] ++ es)) `mplus`
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        es <- expressionList
        return $ Statement p es) `mplus`
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        return $ Statement p None)

return' :: ParseTree Basic
return' = do
    (Symbol r) <- string' "return"
    return $ Statement r None

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

nesting :: ParseTree Basic -> ParseTree Basic
nesting function = StateT $ \s -> if s == Nil then Nothing else
    case s of
        (Cons c@(Cons s s') s'') ->
            case (runStateT function c) of
                Nothing -> Nothing
                Just (res, _) -> Just (res, s'')
        _ -> Nothing

expression :: ParseTree Basic
expression = addExp `mplus` value

expressionList :: ParseTree Basic
expressionList =
    (do
        e <- expression
        (ExpressionList es) <- expressionList
        return $ ExpressionList ([e] ++ es)) `mplus`
    (do
        e <- expression
        return $ ExpressionList [e])

addExp =
    (do
        m <- value
        (Symbol op) <- item'
        if (op `elem` ["+", "-", "*", "^", "/", "=", "<>", ">", ">=", "<", "<="]) then do
            a <- addExp
            return $ Expression op $ ExpressionList [m, a]
            else mzero) `mplus` value

value = (nesting expression) `mplus` function `mplus` variable `mplus` constant

variable = array' `mplus` iD

array' :: ParseTree Basic
array' = do
    (Variable (String' s)) <- iD
    e@(ExpressionList es) <- nesting expressionList
    return $ Array' s (length es) e

function = 
    do 
        (Symbol i) <- item'
        if (i `elem` ["int", "rnd", "log", "abs", "sqrt"]) then do
            e <- nesting expression
            return $ Function i e
            else mzero

constant = (do {(Number i) <- item'; return $ Constant $ Integer' i}) `mplus`
           (do {(Floating f) <- item'; return $ Constant $ Floating' f}) `mplus`
           basicString

printBasic [] = putStr ""
printBasic ((Line l x):xs) = do
    putStr (show l ++ ": ")
    putStrLn (show x)
    printBasic xs

translateSexpr :: Sexpr -> [Basic]
translateSexpr Nil = []
translateSexpr (Cons (Number i) s) = let result = runStateT statements s
                                     in case result of
                                        (Just (r@(StatementList ss), _)) -> [Line i r]
                                        Nothing -> []
translateSexpr (Cons s s') = translateSexpr s ++ translateSexpr s'

renumber :: [Basic] -> Int -> Int -> [(Int, Int)] -> ([Basic], [(Int, Int)])
renumber [] _ _ mapping = ([], mapping)
renumber basic@((Line l b):ls) line newline mapping = if l /= line then renumber basic l (newline + 1) mapping
                                                      else let s = [Line newline b]
                                                               (ys, mapping') = renumber ls line newline mapping
                                                           in if ((l, newline) `elem` mapping) then (s, mapping')
                                                              else (s ++ ys, [(l, newline)] ++ mapping')

-- Begin the actual compiler

generatePush line (Integer' i) = [Push line (VIntegral i)]
generatePush line (Floating' f) = [Push line (VFloating f)]
generatePush line (String' s) = [Push line (VString s)]
generatePush line (Variable (String' v)) = [Push line (VString v), Load line]

renumberLine :: Int -> [(Int, Int)] -> Int
renumberLine line ((orig, new):ls) = if line == orig then new else renumberLine line ls

evalExpressionList line (ExpressionList []) mapping = []
evalExpressionList line (ExpressionList (x:xs)) mapping = evalExpression line x mapping ++ 
                                                          evalExpressionList line (ExpressionList xs) mapping

-- evalExpression deals with things like arithmetic and addition
evalExpression line None mapping = []
evalExpression line (Expression "+" rest) mapping = evalExpression line rest mapping ++ [Add line]
evalExpression line (Expression "-" rest) mapping = evalExpression line rest mapping ++ [Sub line]
evalExpression line (Expression "*" rest) mapping = evalExpression line rest mapping ++ [Mult line]
evalExpression line (Expression "/" rest) mapping = evalExpression line rest mapping ++ [Div line]
evalExpression line (Expression "^" rest) mapping = evalExpression line rest mapping ++ [Pow line]
evalExpression line (Expression "=" rest) mapping = evalExpression line rest mapping ++ [Equal line]
evalExpression line (Expression "<>" rest) mapping = evalExpression line rest mapping ++ [NotEqual line]
evalExpression line (Expression ">" rest) mapping = evalExpression line rest mapping ++ [Greater line]
evalExpression line (Expression "<" rest) mapping = evalExpression line rest mapping ++ [Less line]
evalExpression line (Expression "<=" rest) mapping = evalExpression line rest mapping ++ [LEqual line]
evalExpression line (Expression ">=" rest) mapping = evalExpression line rest mapping ++ [GEqual line]
evalExpression line (Function "int" rest) mapping = evalExpression line rest mapping ++ [CastInt line]
evalExpression line (Function "log" rest) mapping = evalExpression line rest mapping ++ [Log line]
evalExpression line (Function "rnd" rest) mapping = evalExpression line rest mapping ++ [Rand line]
evalExpression line (Function "abs" rest) mapping = evalExpression line rest mapping ++ [Abs line]
evalExpression line (Function "sqrt" rest) mapping = 
    evalExpression line rest mapping ++ generatePush line (Floating' 0.5) ++ [Pow line]
evalExpression line e@(ExpressionList xs) mapping = evalExpressionList line e mapping
evalExpression line s@(Statement _ _) mapping = evalStatement line s mapping
evalExpression line (Constant i) mapping = generatePush line i
evalExpression line v@(Variable i) mapping = generatePush line v

evalIfThenStatement line (Constant (Integer' i)) mapping = 
    let jump = VIntegral $ renumberLine i mapping in
        [Push line (VStatement $ [Push line jump, Goto line])] ++ [IfThen line]
evalIfThenStatement line e mapping = [Push line $ VStatement $ evalExpression line e mapping] ++ [IfThen line]

evalLetStatement line (ExpressionList ((Variable s):xs)) mapping = 
    generatePush line s ++ evalExpressionList line (ExpressionList xs) mapping ++ [Store line]

-- evalStatement deals with things like if, for, goto, etc.
evalStatement line None mapping = []
evalStatement line (Statement "end" _) mapping = [End line]
evalStatement line (Statement "if" e) mapping = evalStatement line e mapping
evalStatement line (Statement "then" e) mapping = evalIfThenStatement line e mapping
evalStatement line (Statement "input" e) mapping = evalExpression line e mapping ++ [Input line]
evalStatement line (Statement "print" e) mapping = evalExpression line e mapping ++ [Print line]
evalStatement line (Statement "print!" e) mapping = evalExpression line e mapping ++ [PrintBang line]
evalStatement line (Statement "let" e) mapping = evalLetStatement line e mapping
evalStatement line (Statement "for" (ExpressionList ((Variable v):es))) mapping = generatePush line v ++ evalExpressionList line (ExpressionList es) mapping
evalStatement line (Statement "tab" e) mapping = evalExpression line e mapping ++ [Spaces line]
evalStatement line (Statement "to" (ExpressionList ((Variable (String' v)):e:es))) mapping =
    evalExpression line e mapping ++ [Store line] ++ [Push line (VString (v ++ "maxrange"))] ++ evalExpressionList line (ExpressionList es) mapping ++ [Store line] ++
    [Push line (VIntegral $ line + 1)] ++ [PushCallstack line]
evalStatement line (Statement "next" (Variable var@(String' v))) mapping =
    generatePush line var ++ generatePush line var ++ [Load line] ++ [Push line (VIntegral 1)] ++ [Add line] ++ [Store line] ++ -- This increments the variable
    generatePush line var ++ [Load line] ++ [Push line (VString (v ++ "maxrange"))] ++ [Load line] ++ [LEqual line] ++ -- This performs the comparison to see if the loop is done
    [Push line $ VStatement $ [ -- Creating code which can jump to the top of the loop if necessary
        Push line (VString (v ++ "jmp")), PopCallstack line, Store line, -- Stores the jump location in variable (v ++ jmp)
        Push line (VString (v ++ "jmp")), Load line, PushCallstack line, -- Loads the jump location to the stack and pushes it back to the callstack (for the next iteration to see it again)
        Push line (VString (v ++ "jmp")), Load line, Goto line -- Loads the jump location onto the stack and jumps to it
        ]] ++ [IfThen line] ++ [Push line (VString (v ++ "jmp"))] ++ [PopCallstack line] ++ [Store line] -- IfThen compares the result of checking if the loop is done, and if it isn't it executes the code which restarts the loop at the top
evalStatement line (Statement "goto" (Integer' i)) mapping = 
    generatePush line (Integer' $ renumberLine i mapping) ++ [Goto line]
evalStatement line (Statement "gosub" (Integer' i)) mapping = 
    let renumbered = renumberLine i mapping 
        nextLine = line + 1 in
        generatePush line (Integer' renumbered) ++ generatePush line (Integer' nextLine) ++
        [PushCallstack line] ++ [Goto line]
evalStatement line (Statement "return" _) mapping = [PopCallstack line] ++ [Goto line]
evalStatement line e@(ExpressionList _) mapping = evalExpressionList line e mapping
evalStatement line e@(Expression _ _) mapping = evalExpression line e mapping

--evalStatement :: Basic -> [(Int, Int)] -> [Bytecode]
evalStatementList _ None mapping = []
evalStatementList _ (StatementList []) mapping = []
evalStatementList line (StatementList (s:ss)) mapping = evalStatement line s mapping ++ 
                                                        evalStatementList line (StatementList ss) mapping

compile' :: [Basic] -> [(Int, Int)] -> [Bytecode]
compile' [] mapping = []
compile' (b@(Line l s):bs) mapping = evalStatementList l s mapping ++ compile' bs mapping

compile :: Sexpr -> [Bytecode]
compile s = case s of
    (Symbol s) -> []
    _ -> let (basic, mapping) = renumber (translateSexpr s) 0 (-1) [] in
        compile' basic mapping
