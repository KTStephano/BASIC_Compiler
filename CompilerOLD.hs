module CompilerOLD
(Sexpr(Symbol, Number, Floating, Nil, Cons), 
 Value(VIntegral, VFloating, VString, VSymbol, VBool, VStatement, Null), 
 Bytecode(End, Push, Print, PrintBang, Add, Mult, Sub, Div, Load, Store, Input, Equal, NotEqual, Greater, GEqual, Less, LEqual, IfThen, Goto), 
 analyze, car, cdr, compile, printBytecode, line) where

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe

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
    "(250 return )))"

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

extractQuotes xs = filter (\s -> s /= '\"') xs

generatePush line (Floating d) = [Push line (VFloating d)]
generatePush line (Number i) = [Push line (VIntegral i)]
-- This is a string constant, such as "hello"
generatePush line (Symbol s@('"':xs)) = [Push line (VString (extractQuotes s))]
-- This is probably a variable
generatePush line (Symbol s) = [Push line (VString s), Load line]

-- Parses the Let statement
evalLetArgs :: Int -> Sexpr -> [Bytecode]
evalLetArgs line (Cons (Symbol "=") s) = evalExpr line s
evalLetArgs line (Cons x@(Symbol s) s') = [Push line (VString s)] ++ evalLetArgs line s'

-- Returns bytecode that pushes a VStatement onto the stack for later execution
evalStatement :: Int -> Sexpr -> [Bytecode]
evalStatement line (Cons (Number i) Nil) = [Push line (VStatement [Push line (VIntegral i), Goto line])]
evalStatement line s = [Push line $ VStatement $ evalExpr line s]

evalExpr :: Int -> Sexpr -> [Bytecode]
evalExpr line (Cons (Symbol "end") s) = [End line]
evalExpr line (Cons (Symbol "print") s) = evalExpr line s ++ [Print line]
evalExpr line (Cons (Symbol "print!") s) = evalExpr line s ++ [PrintBang line]
evalExpr line (Cons (Symbol "+") s) = evalExpr line s ++ [Add line]
evalExpr line (Cons (Symbol "-") s) = evalExpr line s ++ [Sub line]
evalExpr line (Cons (Symbol "*") s) = evalExpr line s ++ [Mult line]
evalExpr line (Cons (Symbol "/") s) = evalExpr line s ++ [Div line]
evalExpr line (Cons (Symbol "=") s) = evalExpr line s ++ [Equal line]
evalExpr line (Cons (Symbol "<>") s) = evalExpr line s ++ [NotEqual line]
evalExpr line (Cons (Symbol ">") s) = evalExpr line s ++ [Greater line]
evalExpr line (Cons (Symbol "<") s) = evalExpr line s ++ [Less line]
evalExpr line (Cons (Symbol "<=") s) = evalExpr line s ++ [LEqual line]
evalExpr line (Cons (Symbol ">=") s) = evalExpr line s ++ [GEqual line]
evalExpr line (Cons (Symbol "then") s) = evalStatement line s ++ [IfThen line]
evalExpr line (Cons (Symbol "if") s) = evalExpr line s
evalExpr line (Cons (Symbol "goto") s) = evalExpr line s ++ [Goto line]
evalExpr line (Cons (Symbol "gosub") s) = evalExpr line s ++ [NextLine line] ++ [PushCallstack line] ++ [Goto line]
evalExpr line (Cons (Symbol "return") s) = [PopCallstack line] ++ [Goto line]
--evalExpr line (Cons (Cons (Symbol "let") s) s') = evalLetArgs line s' ++ [Let line]
evalExpr line (Cons (Symbol "let") s) = evalLetArgs line s ++ [Store line]
evalExpr line (Cons (Symbol "input") s) = evalExpr line s ++ [Input line]
evalExpr line (Cons x@(Number i) s) = generatePush line x ++ evalExpr line s
evalExpr line (Cons x@(Floating f) s) = generatePush line x ++ evalExpr line s
evalExpr line (Cons x@(Symbol xs) s) = generatePush line x ++ evalExpr line s -- Symbol containing a quoted value
--evalExpr line (Cons (Cons i s) s') = [generatePush i] ++ evalExpr line s ++ evalExpr line s'
evalExpr line (Cons s s') = evalExpr line s ++ evalExpr line s'
evalExpr line _ = []

compile :: Sexpr -> [Bytecode]
compile (Cons (Number i) s) = evalExpr i s -- For this one, (Number i) is the line number and s contains the function + args
compile (Cons s1 s2) = (compile s1) ++ (compile s2)
compile _ = []

printBytecode :: [Bytecode] -> IO ()
printBytecode [] = putStr ""
printBytecode (x:xs) = do
    putStrLn (show x)
    printBytecode xs