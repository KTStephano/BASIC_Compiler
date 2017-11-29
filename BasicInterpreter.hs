-- * = 0 or mroe
-- + = 1 or more
-- {} = symbolic class or something
-- lower = lowercase digit
-- digit = number

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe

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
    let miscVals = ['<', '>', '^', '+', '-', '*', '/', '=', '!']
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

pr = "(define pr '((100 print! \"hello\")" ++
                  "(200 print \" goodbyte\")" ++
                  "(300 print \"Just kidding\" \" no really\")" ++
                  "(400 print (1 + (1 + 2))))))"

pr2 = "(define pr '(100 print ((2 * 3) - (4 * (5 * 6)))))"
pr3 = "(define pr '((100 print ((1 + 4) + (2 + 3)))))"

lettest = "(define ltest '((100 let x = ((2 * 3) - (4 * (5 * 6))))" ++
                          "(200 print x)))"

-- Parses the string and returns it in a form that is compiler-friendly
analyze :: [Char] -> Sexpr
analyze str = let result = p str
              in case result of 
                (Symbol s) -> Symbol s -- Parser returned an error
                s -> car $ cdr $ cdr s

data Value = VIntegral Int | VFloating Double | VString String deriving (Eq)

instance Show Value where
    show (VIntegral x) = show x
    show (VFloating d) = show d
    show (VString s) = s

newtype Environment = Environment {getEnv :: [(String, Value)]} deriving (Show, Eq)

findEnv :: Environment -> String -> [(String, Value)]
findEnv (Environment []) _ = []
findEnv (Environment ((s,v):xs)) str = if s == str then [(s,v)] else findEnv (Environment xs) str

updateEnv :: Environment -> String -> Value -> Environment
updateEnv env str val = let x = findEnv env str
                        in if (x == []) then Environment $ (getEnv env) ++ [(str, val)]
                           else Environment $ (filter (\y -> y /= (x !! 0)) $ getEnv env) ++ [(str, val)]

data Bytecode = End {line :: Int} | Push {arg :: Value} | Print {line :: Int} | PrintBang {line :: Int} | 
                Add {line :: Int} | Mult {line :: Int} | Sub {line :: Int} | Load {line :: Int} | Store {line :: Int} |
                Input {line :: Int} deriving (Show)

data Frame = Frame {getStack :: [Value]} deriving (Show)

getStackValue (Frame xs) index = xs !! index

getStackLength (Frame xs) = length xs

extractQuotes xs = filter (\s -> s /= '\"') xs

generatePush line (Floating d) = [Push (VFloating d)]
generatePush line (Number i) = [Push (VIntegral i)]
-- This is a string constant, such as "hello"
generatePush line (Symbol s@('"':xs)) = [Push (VString (extractQuotes s))]
-- This is probably a variable
generatePush line (Symbol s) = [Push (VString s), Load line]

{-
extractArgs :: Sexpr -> [Bytecode]
extractArgs Nil = []
extractArgs (Cons s s') = [generatePush s] ++ extractArgs s'

extractFunction :: Int -> Sexpr -> [Bytecode]
extractFunction line (Cons (Symbol "end") s) = [End line]
extractFunction line (Cons (Symbol "print") s) = extractExpr line s ++ [Print line]
extractFunction line (Cons (Symbol "print!") s) = extractExpr line s ++ [PrintBang line]

extractExpr line (Cons (Cons (Number i) (Cons (Symbol "+") s)) s') = [generatePush (Number i)] ++ extractExpr line s ++ [Add line]
extractExpr line s = extractArgs s
-}

-- Parses the Let statement
evalLetArgs :: Int -> Sexpr -> [Bytecode]
evalLetArgs line (Cons (Symbol "=") s) = evalExpr line s
evalLetArgs line (Cons x@(Symbol s) s') = [Push (VString s)] ++ evalLetArgs line s'

evalExpr :: Int -> Sexpr -> [Bytecode]
evalExpr line (Cons (Symbol "end") s) = [End line]
evalExpr line (Cons (Symbol "print") s) = evalExpr line s ++ [Print line]
evalExpr line (Cons (Symbol "print!") s) = evalExpr line s ++ [PrintBang line]
evalExpr line (Cons (Symbol "+") s) = evalExpr line s ++ [Add line]
evalExpr line (Cons (Symbol "-") s) = evalExpr line s ++ [Sub line]
evalExpr line (Cons (Symbol "*") s) = evalExpr line s ++ [Mult line]
--evalExpr line (Cons (Cons (Symbol "let") s) s') = evalLetArgs line s' ++ [Let line]
evalExpr line (Cons (Symbol "let") s) = evalLetArgs line s ++ [Store line]
evalExpr line (Cons (Symbol "input") s) = evalExpr line s ++ [Input line]
evalExpr line (Cons x@(Number i) s) = generatePush line x ++ evalExpr line s
evalExpr line (Cons x@(Floating f) s) = generatePush line x ++ evalExpr line s
evalExpr line (Cons x@(Symbol xs) s) = generatePush line x ++ evalExpr line s -- Symbol containing a quoted value
--evalExpr line (Cons (Cons i s) s') = [generatePush i] ++ evalExpr line s ++ evalExpr line s'
evalExpr line (Cons s s') = evalExpr line s ++ evalExpr line s'
evalExpr line _ = []

compile :: Sexpr -> [Bytecode] -> [Bytecode]
compile (Cons (Number i) s) code = evalExpr i s -- For this one, (Number i) is the line number and s contains the function + args
compile (Cons s1 s2) code = (compile s1 code) ++ (compile s2 code)
compile _ code = []

load :: Frame -> Environment -> Frame
load frame env = let (frame', (VString var)) = pop frame
                     val = findEnv env var
                 in case val of
                    [] -> frame
                    (x:xs) -> push frame' (snd x)

store :: Frame -> Environment -> (Frame, Environment)
store frame env = let (frame', val) = pop frame
                      (frame'', (VString var)) = pop frame'
                  in (frame'', updateEnv env var val)

push :: Frame -> Value -> Frame
push frame val = Frame $ (getStack frame) ++ [val]

pop :: Frame -> (Frame, Value)
pop frame = let xs = reverse $ getStack frame
                x = head xs
            in (Frame $ reverse $ tail xs, x)

add (VIntegral i) (VIntegral j) = VIntegral $ i + j
add (VFloating i) (VFloating j) = VFloating $ i + j

sub (VIntegral i) (VIntegral j) = VIntegral $ i - j
sub (VFloating i) (VFloating j) = VFloating $ i - j

mult (VIntegral i) (VIntegral j) = VIntegral $ i * j
mult (VFloating i) (VFloating j) = VFloating $ i * j

inputHelper str = do
    putStrLn (str ++ "?")
    s <- getLine
    return s

input frame env = let (frame', (VString var)) = pop frame
                      (frame'', (VString str)) = pop frame'
                  in (do
                    putStrLn (str ++ "?")
                    s <- getLine
                    let env' = updateEnv env var (VIntegral (read s :: Int))
                    return (frame'', env')) 
                    <|>
                    (do
                        putStrLn (str ++ "?")
                        s <- getLine
                        let env' = updateEnv env var (VFloating (read s :: Double))
                        return (frame'', env'))
                    <|>
                    (do
                        putStrLn (str ++ "?")
                        s <- getLine
                        let env' = updateEnv env var (VString s)
                        return (frame'', env')) 


--print' (Frame []) = "\n"
print' (Frame []) = putStrLn ""
print' (Frame (x:xs)) = do
    putStr (show x)
    print' (Frame xs)

printBang (Frame []) = putStr ""
printBang (Frame (x:xs)) = do
    putStr (show x)
    printBang (Frame xs)

vm :: [Bytecode] -> Environment -> [Bytecode] -> Frame -> IO ()
vm program env [] frame = putStr ""
vm program env ((End l):rest) frame = putStr ""
vm program env ((Push a):rest) frame = vm program env rest (push frame a)
vm program env ((Input l):rest) frame = do
    (frame', env') <- input frame env
    vm program env' rest frame'
vm program env ((Load l):rest) frame = do
    let frame' = load frame env
    vm program env rest frame'
vm program env ((Store l):rest) frame = do
    let (frame', env') = store frame env
    vm program env' rest frame'
vm program env ((Add l):rest) frame = do
    let (frame', x) = pop frame
    let (frame'', y) = pop frame'
    let result = add y x
    let frame''' = push frame'' result
    vm program env rest frame'''
vm program env ((Sub l):rest) frame = do
    let (frame', x) = pop frame
    let (frame'', y) = pop frame'
    let result = sub y x
    let frame''' = push frame'' result
    vm program env rest frame'''
vm program env ((Mult l):rest) frame = do
    let (frame', x) = pop frame
    let (frame'', y) = pop frame'
    let result = mult y x
    let frame''' = push frame'' result
    vm program env rest frame'''
vm program env ((PrintBang l):rest) frame = do
    printBang frame
    vm program env rest (Frame [])
vm program env ((Print l):rest) frame = do
    print' frame
    vm program env rest (Frame [])