-- * = 0 or mroe
-- + = 1 or more
-- {} = symbolic class or something
-- lower = lowercase digit
-- digit = number

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Sexpr = Symbol String | Number Int | Floating Double | Nil | Cons Sexpr Sexpr deriving (Eq)

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
    x <- symb "\""
    y <- (many quotedString)
    z <- symb "\""
    return (x ++ y ++ z)

quotedString = alphanum +++ (sat isSpace)

cdigit = do 
    c <- sat isDigit
    return c

integernum = do
    r <- many1 cdigit
    return (read r :: Int)

number = do
    r <- many1 cdigit
    d <- symb "."
    l <- many1 cdigit
    return (read (r ++ d ++ l) :: Double)

misc = do
    r <- item--token item
    let miscVals = ['<', '>', '^', '+', '-', '*', '/', '=']
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
a = (do
    s <- symbol
    return $ Symbol s) +++
        (do
            n <- number
            return $ Floating n) +++
            (do
                n <- integernum
                return $ Number n)

p str = let result = parse s str
        in if (result == []) then Symbol "Error parsing string"
           else fst (result !! 0)

foo = "(define foo " ++
      "'((100 input \"What is the value of A\" a )" ++
      " (110 input \"What is the value of B\" b )" ++
      " (120 input \"What is the value of C\" c )" ++
      " (130 let d = ((b * b) - (4.0 * (a * c))) )" ++
      " (140 print d) (150 end)))"

pr = "(define pr '((100 print \"hello\")))"

data Value = VIntegral Int | VFloating Double | VString String deriving (Show, Eq)

newtype Environment = Environment {getEnv :: [(String, Value)]} deriving (Show, Eq)

findEnv :: Environment -> String -> [(String, Value)]
findEnv (Environment []) _ = []
findEnv (Environment ((s,v):xs)) str = if s == str then [(s,v)] else findEnv (Environment xs) str

updateEnv :: Environment -> String -> Value -> Environment
updateEnv env str val = let x = findEnv env str
                        in if (x == []) then Environment $ (getEnv env) ++ [(str, val)]
                           else Environment $ (filter (\y -> y /= (x !! 0)) $ getEnv env) ++ [(str, val)]

data Bytecode = Return {line :: Int} | Push {arg :: Value} | Pop | Print {line :: Int} deriving (Show)

extractArgs :: Sexpr -> [Bytecode]
extractArgs Nil = []
extractArgs (Cons (Floating d) s') = [Push (VFloating d)] ++ extractArgs s'
extractArgs (Cons (Number i) s') = [Push (VIntegral i)] ++ extractArgs s'
extractArgs (Cons (Symbol s) s') = [Push (VString s)] ++ extractArgs s'

extractFunction :: Int -> Sexpr -> [Bytecode]
extractFunction line (Cons (Symbol "return") s) = extractArgs s ++ [Return line]
extractFunction line (Cons (Symbol "print") s) = extractArgs s ++ [Print line]

compile :: Sexpr -> [Bytecode] -> [Bytecode]
compile Nil code = []
compile (Cons (Number i) s) code = extractFunction i s
compile (Cons s1 s2) code = (compile s1 code) ++ (compile s2 code)
compile _ code = []