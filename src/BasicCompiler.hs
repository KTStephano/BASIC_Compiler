{-
    This is a multi-stage (basic -> bytecode) compiler. The stages are roughly defined as follows:

        Stage 1) Input is parsed and translated into an S-expression
        Stage 2) S-expression itself is traversed and translated into a BASIC syntax tree
        Stage 3) BASIC syntax tree is traversed to find any calls to "data", and these are
                 then placed at the top and combined into a single statement to execute before anything else
        Stage 4) BASIC syntax tree is traversed again and renumbered
        Stage 5) Compiler traverses the BASIC syntax tree one final time and compiles it to Bytecode which
                 is ready for the virtual machine to interpret
    
    These stages are reasonably in order code-wise from what is described above (i.e. first section is type
    definitions, next section is Sexpr parser, next is Sexpr -> BASIC parser, final section is BASIC -> Bytecode compiler, etc.).
-}

module BasicCompiler
 (Sexpr(..),
  Value(..),
  Bytecode(..),
  analyze, car, cdr, compile, printBytecode) where

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe
import Data.IORef
import Control.Monad.Trans.State
import System.Environment
import System.IO

data Sexpr = Symbol String | Number Integer | Floating Double | Nil | Cons Sexpr Sexpr deriving (Eq)

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

data Bytecode = End {line :: Integer} | Push {line :: Integer, arg :: Value} | Print {line :: Integer} | PrintBang {line :: Integer} | 
                Add {line :: Integer} | Mult {line :: Integer} | Sub {line :: Integer} | Div {line :: Integer} | Pow {line :: Integer} |
                Load {line :: Integer} | Store {line :: Integer} | Input {line :: Integer} | Equal {line :: Integer} | 
                NotEqual {line :: Integer} | Greater {line :: Integer} | GEqual {line :: Integer} | Less {line :: Integer} | 
                LEqual {line :: Integer} | IfThen {line :: Integer} | Goto {line :: Integer} | NextLine {line :: Integer} | 
                PushCallstack {line :: Integer} | PopCallstack {line :: Integer} | Spaces {line :: Integer} | CastInt {line :: Integer} |
                Rand {line :: Integer} | Log {line :: Integer} | Abs {line :: Integer} | And {line :: Integer} | Or {line :: Integer} |
                ALoad {line :: Integer} | ALoad2D {line :: Integer} | NewArray {line :: Integer} | NewArray2D {line :: Integer} |
                AStore {line :: Integer} | OnGoto {line :: Integer} | OnGosub {line :: Integer} | Length {line :: Integer} | 
                Substring {line :: Integer} | Sin {line :: Integer} | Cos {line :: Integer} | Tan {line :: Integer} | 
                ArcSin {line :: Integer} | ArcCos {line :: Integer} | ArcTan {line :: Integer} | SubstrLast {line :: Integer} |
                NoOp {line :: Integer} deriving (Eq)

instance Show Bytecode where
    show (End l) = "end"
    show (Push l a) = "push " ++ show a
    show (Print l) = "print"
    show (PrintBang l) = "printbang"
    show (Add l) = "add"
    show (Mult l) = "mult"
    show (Sub l) = "sub"
    show (Div l) = "div"
    show (Pow l) = "pow"
    show (Load l) = "load"
    show (Store l) = "store"
    show (Input l) = "input"
    show (Equal l) = "equal"
    show (NotEqual l) = "notequal"
    show (Greater l) = "greater"
    show (GEqual l) = "gequal"
    show (Less l) = "less"
    show (LEqual l) = "lequal"
    show (IfThen l) = "ifthen"
    show (Goto l) = "goto"
    show (NextLine l) = "nextline"
    show (PushCallstack l) = "pushcallstack"
    show (PopCallstack l) = "popcallstack"
    show (Spaces l) = "spaces"
    show (CastInt l) = "castint"
    show (Rand l) = "rand"
    show (Log l) = "log"
    show (Abs l) = "abs"
    show (And l) = "and"
    show (Or l) = "or"
    show (ALoad l) = "aload"
    show (ALoad2D l) = "aload2d"
    show (NewArray l) = "newarray"
    show (NewArray2D l) = "newarray2d"
    show (AStore l) = "astore"
    show (OnGoto l) = "ongoto"
    show (OnGosub l) = "ongosub"
    show (Length l) = "length"
    show (Substring l) = "substr"
    show (Sin l) = "sin"
    show (Cos l) = "cos"
    show (Tan l) = "tan"
    show (ArcSin l) = "arcsin"
    show (ArcCos l) = "arccos"
    show (ArcTan l) = "arctan"
    show (SubstrLast l) = "substrlast"
    show (NoOp l) = "noop"

data Value = VIntegral Integer | VFloating Double | VString String | VSymbol {name :: String, val :: Value} |
             VBool Bool | VStatement [Bytecode] | VIntegerList [Integer] | Null | VPair (Value, Value) |
             VDataRef (IORef Value) | VList [Value]

instance Eq Value where
    (VIntegral i) == (VIntegral ii) = i == ii
    (VIntegral i) == (VFloating f) = (fromIntegral i) == f
    (VFloating f) == (VIntegral i) = f == (fromIntegral i)
    (VFloating f) == (VFloating ff) = f == ff
    (VBool b) == (VBool bb) = b == bb
    (VList ls) == (VList ls') = ls == ls'
    (VString s) == (VString ss) = map toLower s == map toLower ss
    Null == Null = True
    Null == _ = False
    _ == Null = False

instance Ord Value where
    (VIntegral i) <= (VIntegral ii) = i <= ii
    (VIntegral i) <= (VFloating f) = (fromIntegral i) <= f
    (VFloating f) <= (VIntegral i) = f <= (fromIntegral i)
    (VFloating f) <= (VFloating ff) = f <= ff
    (VBool b) <= (VBool bb) = b <= bb
    (VString s) <= (VString ss) = s <= ss
    (VList ls) <= (VList ls') = ls <= ls'

instance Fractional Value where
    (VIntegral i) / (VIntegral ii) = VFloating $ (fromIntegral i) / (fromIntegral ii)
    (VIntegral i) / (VFloating f) = VFloating $ (fromIntegral i) / f
    (VFloating f) / (VIntegral i) = VFloating $ f / (fromIntegral i)
    (VFloating f) / (VFloating ff) = VFloating $ f / ff

    fromRational f = VFloating $ fromRational f

instance Num Value where
    (VIntegral i) + (VIntegral ii) = VIntegral $ i + ii
    (VIntegral i) + (VFloating f) = VFloating $ (fromIntegral i) + f
    (VFloating f) + (VIntegral i) = VFloating $ f + (fromIntegral i)
    (VFloating f) + (VFloating ff) = VFloating $ f + ff
    (VString s) + (VString ss) = VString $ s ++ ss
    (VList ls) + (VList ls') = VList $ ls ++ ls'

    (VIntegral i) * (VIntegral ii) = VIntegral $ i * ii
    (VIntegral i) * (VFloating f) = VFloating $ (fromIntegral i) * f
    (VFloating f) * (VIntegral i) = VFloating $ f * (fromIntegral i)
    (VFloating f) * (VFloating ff) = VFloating $ f * ff

    abs (VIntegral i) = VIntegral $ abs (fromIntegral i)
    abs (VFloating f) = VFloating $ abs f

    signum (VIntegral i) = if i == 0 then VIntegral 0
                           else if i > 0 then VIntegral 1 else VIntegral (-1)
    signum (VFloating f) = if f == 0.0 then VFloating 0.0
                           else if f > 0.0 then VFloating 1.0 else VFloating (-1.0)

    fromInteger i = VIntegral (fromIntegral i)

    negate (VIntegral i) = VIntegral $ i * (-1)
    negate (VFloating f) = VFloating $ f * (-1.0)

instance Show Value where
    show (VIntegral x) = show x
    show (VFloating d) = show d
    show (VString s) = s
    show (VBool b) = show b
    show Null = "Null"
    show (VSymbol vr vl) = show vl
    show (VStatement s) = show s
    show (VIntegerList is) = show is
    show (VDataRef v) = "#reference"
    show (VList ls) = show ls

data Basic = String' String | Integer' Integer | Floating' Double | StatementList [Basic] |
             Line Integer Basic | Lines [Basic] | Variable Basic | Function String Basic | Value Basic | 
             Constant Basic | Statement {getName :: String, body :: Basic} | 
             Expression String Basic | ExpressionList [Basic] | 
             Array' String Integer Basic | IntegerList [Basic] | None deriving (Eq)

-- This helps us implement (super simple) compile-time type checking
data Type = TVariable {typeid :: String} | TArray {typeid :: String} deriving (Show, Eq)

instance Show Basic where
    show (String' s) = s
    show (Integer' i) = show i
    show (Floating' f) = show f
    show (StatementList xs) = "{StatementList " ++ show xs ++ "}"
    show (Line l b) = show l ++ ": " ++ show b
    show (Variable b) = "Var " ++ show b
    show (Function s b) = "Function " ++ s ++ " -> " ++ show b
    show (Array' n i b) = "Array -> " ++ n ++ " " ++ show i ++ " " ++ show b
    show (Statement s b) = "{Statement " ++ s ++ " -> " ++ show b ++ "}"
    show (Expression s b) = "{Expression " ++ s ++ " -> " ++ show b ++ "}"
    show (ExpressionList xs) = "{ExpressionList " ++ show xs ++ "}"
    show (Constant c) = "Const " ++ show c
    show (IntegerList is) = "{IntegerList -> " ++ show is ++ "}"
    show None = "None"

type ParseTree a = StateT (Sexpr, [Type]) Maybe a

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

quotedString = alphanum +++ (sat isSpace) +++ quotedMisc

quotedMisc = do
    r <- item--token item
    if (r `elem` ['"']) then mzero else
        if (isPrint r == True) then return r else mzero

cdigit = do 
    c <- sat isDigit
    return c

integernum = (do
    r <- many1 cdigit
    return (read r :: Integer)) +++
    (do
        s <- char '-'
        r <- many1 cdigit
        return (read ([s] ++ r) :: Integer))

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

item' :: ParseTree Sexpr
item' = StateT $ \(s, t) -> if s == Nil then Nothing else 
    case (car s) of
        Nil -> Nothing
        s' -> Just (s', (cdr s, t))

string' cs = do {i <- item'; if (Symbol cs) == i then return i else mzero}

symb' :: Sexpr -> ParseTree Sexpr
symb' cs = do {i <- item'; if cs == i then return i else mzero}

-- Functions for converting a Sexpr (parse tree) into Basic

lookupEnv'' s@(Symbol s') [] = Nothing
lookupEnv'' s@(Symbol s') (t:ts) = if typeid t == s' then Just t else lookupEnv'' s ts

lookupEnv' s@(Symbol _) = StateT $ \(s', t) ->
    case (lookupEnv'' s t) of
        Nothing -> Nothing
        Just t' -> Just (t', (s', t))

modifyEnv tid v = StateT $ \(s, t) ->
    let entry = runStateT (lookupEnv' tid) (s, t) in
        case entry of
            Nothing -> Just (v, (s, [v] ++ t))
            Just (t', _) -> Just (v, (s, [v] ++ (filter (\x -> typeid x /= typeid v) t)))

iD = do
    (Symbol (s:ss)) <- item'
    if s == '"' then mzero
    else if isLetter s == False then mzero
    else return $ Variable $ String' (s:ss)

basicString =
    do
        (Symbol s@('"':xs)) <- item'
        return $ Constant $ String' s

integer' :: ParseTree Basic
integer' = do
    (Number i) <- item'
    return $ Constant $ Integer' i

integerList :: ParseTree Basic
integerList =
    (do
        i <- integer'
        (IntegerList is) <- integerList
        return $ IntegerList $ [i] ++ is) `mplus`
    (do
        i <- integer'
        return $ IntegerList [i])

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
statement = dim `mplus` endRestore `mplus` for `mplus` goto `mplus` if' `mplus` input `mplus` let' `mplus` next `mplus` on' `mplus` print' `mplus` return' `mplus` setVar `mplus` data' `mplus` read'

rem :: ParseTree Basic
rem = do
    (Symbol r) <- string' "rem"
    return None

dim :: ParseTree Basic
dim = do
    (Symbol d) <- string' "dim"
    a@(Array' s i e) <- array'
    modifyEnv (Symbol s) (TArray s)
    return $ Statement d a

endRestore :: ParseTree Basic
endRestore = do
    (Symbol e) <- (string' "end" `mplus` string' "restore")
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
    e <- (nesting statement `mplus` statement `mplus` expression)
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

read' :: ParseTree Basic
read' = do
    (Symbol r) <- string' "read"
    i <- variable
    return $ Statement r i

data' :: ParseTree Basic
data' = do
    (Symbol d) <- string' "data"
    es <- expressionList
    return $ Statement d es

on' :: ParseTree Basic
on' =
    do
        (Symbol o) <- string' "on"
        e <- expression
        (Symbol g) <- item'
        if (g `elem` ["goto", "gosub"]) then do
            is <- integerList
            return $ Statement o $ ExpressionList [e, is, Statement g None]
        else mzero

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
nesting function = StateT $ \(s, t) -> if s == Nil then Nothing else
    case s of
        (Cons c@(Cons s s') s'') ->
            case (runStateT function (c, t)) of
                Nothing -> Nothing
                Just (res, _) -> Just (res, (s'', t))
        _ -> Nothing

expression :: ParseTree Basic
expression = 
    (do
        a <- addExp
        (Symbol s) <- item'
        if (s `elem` ["and", "or"]) then do
            e <- expression
            return $ Expression s $ ExpressionList [a, e]
            {-
            case e of
                (Expression op@"or" (ExpressionList (l:es))) -> return $ Expression op $ ExpressionList $ [Expression s $ ExpressionList [a, l]] ++ es
                (Expression op@"and" (ExpressionList (l:es))) -> return $ Expression op $ ExpressionList $ [Expression s $ ExpressionList [a, l]] ++ es
                _ -> return $ Expression s $ ExpressionList [a, e]
            -}
        else mzero) `mplus`
    addExp `mplus` value

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

--variable = array' `mplus` iD
variable =
    (do
        st <- get
        a@(Array' s i e) <- array'
        t <- lookupEnv' (Symbol s) -- if this fails we don't continue
        case t of
            (TArray _) -> return a
            _ -> do {put st; mzero}) `mplus`
    (do
        v@(Variable (String' s)) <- iD
        modifyEnv (Symbol s) (TVariable s)
        return v)

array' :: ParseTree Basic
array' = do
    (Variable (String' s)) <- iD
    e@(ExpressionList es) <- nesting expressionList
    return $ Array' s (toInteger $ length es) e

function = 
    do 
        (Symbol i) <- item'
        if (i `elem` ["int", "rnd", "log", "abs", "sqrt", "len", "mid$", 
                      "sin", "cos", "tan", "asin", "acos", "atan", "left$", "right$"]) then do
            if i == "mid$" || i == "left$" || i == "right$" then do
                e <- nesting expressionList
                return $ Function i e
            else do
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
printBasic (x:xs) = do
    putStrLn (show x)
    printBasic xs

translateSexpr' :: Sexpr -> [Type] -> ([Basic], [Type])
translateSexpr' Nil env = ([], env)
translateSexpr' (Cons (Number i) s) env = let result = runStateT statements (s, env)
                                          in case result of
                                            Just (r@(StatementList ss), (s,e)) -> ([Line i r], e)
                                            Nothing -> ([None], env)
translateSexpr' (Cons s s') env = let (l, e) = translateSexpr' s env
                                      (r, e') = translateSexpr' s' e
                                  in (l ++ r, e')

translateSexpr :: Sexpr -> [Basic]
translateSexpr s = let (result, env) = translateSexpr' s [] in
    result

combineDataStatements :: [Basic] -> Basic
combineDataStatements data' = 
    let helper [] acc = acc
        helper ((Line i (StatementList s)):lines) acc = let result = helper s acc in helper lines result
        helper ((Statement _ (ExpressionList es)):d) 
               (Statement _ (ExpressionList stream)) = helper d (Statement "data" (ExpressionList $ stream ++ es))
    in Line 0 $ StatementList $ [helper data' (Statement "data" (ExpressionList []))]

reorderBasic' :: [Basic] -> [Basic] -> [Basic] -> ([Basic], [Basic])
reorderBasic' [] data' acc = (data', acc)
reorderBasic' (d@(Statement "data" (ExpressionList es)):basic) data' acc = 
    reorderBasic' basic (data' ++ [d]) acc
reorderBasic' (b:basic) data' acc = reorderBasic' basic data' (acc ++ [b])

--reorderBasic'' :: [Basic] -> [Basic]
reorderBasic'' [] = ([], [])
reorderBasic'' (None:rest) = reorderBasic'' rest
reorderBasic'' ((Line i (StatementList s)):rest) = let (data', rest') = reorderBasic' s [] []
                                                       (data'', rest'') = reorderBasic'' rest
                                                   in ((if data' == [] then [] else [Line 0 $ StatementList data']) ++ data'',
                                                       (if rest' == [] then [] else [Line i $ StatementList rest']) ++ rest'')

reorderBasic basic = let (data', rest) = reorderBasic'' basic
                     in (if data' == [] then [] else [combineDataStatements data']) ++ rest
                     

renumber :: [Basic] -> Integer -> Integer -> [(Integer, Integer)] -> ([Basic], [(Integer, Integer)])
renumber [] _ _ mapping = ([], mapping)
renumber (None:ls) line newline mapping = renumber ls line newline mapping
renumber basic@((Line l b):ls) line newline mapping = if l /= line then renumber basic l (newline + 1) mapping
                                                      else let s = [Line newline b]
                                                               (ys, mapping') = renumber ls line newline mapping
                                                           in if ((l, newline) `elem` mapping) then (s, mapping')
                                                              else (s ++ ys, [(l, newline)] ++ mapping')

-- Begin the actual compiler

removeQuotes (String' s) = filter (\c -> c /= '"' && c /= '\"') s
removeQuotes (Variable (String' v)) = removeQuotes (String' v)

generatePush line (Constant c) mapping = generatePush line c mapping
generatePush line (Integer' i) mapping = [Push line (VIntegral i)]
generatePush line (Floating' f) mapping = [Push line (VFloating f)]
generatePush line s'@(String' s) mapping = [Push line (VString $ removeQuotes s')]
generatePush line (Variable v'@(String' v)) mapping = [Push line (VString $ removeQuotes v'), Load line]
generatePush line a@(Array' _ i _) mapping =
    if (i == 1) then generateSimpleArrayPush line a mapping ++ [ALoad line]
    else generateSimpleArrayPush line a mapping ++ [ALoad2D line]

-- Will push the array name and indices, but will not append a load/store operation after
generateSimpleArrayPush line (Array' s i b) mapping =
    if (i == 1) then [Push line (VString s)] ++ evalExpression line b mapping
    else [Push line (VString s)] ++ evalExpression line b mapping

renumberLine :: Integer -> [(Integer, Integer)] -> Integer
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
evalExpression line (Expression "and" rest) mapping = evalExpression line rest mapping ++ [And line]
evalExpression line (Expression "or" rest) mapping = evalExpression line rest mapping ++ [Or line]
evalExpression line (Function "int" rest) mapping = evalExpression line rest mapping ++ [CastInt line]
evalExpression line (Function "log" rest) mapping = evalExpression line rest mapping ++ [Log line]
evalExpression line (Function "rnd" rest) mapping = evalExpression line rest mapping ++ [Rand line]
evalExpression line (Function "abs" rest) mapping = evalExpression line rest mapping ++ [Abs line]
evalExpression line (Function "len" rest) mapping = evalExpression line rest mapping ++ [Length line]
evalExpression line (Function "mid$" rest) mapping = evalExpression line rest mapping ++ [Substring line]
evalExpression line (Function "sin" rest) mapping = evalExpression line rest mapping ++ [Sin line]
evalExpression line (Function "cos" rest) mapping = evalExpression line rest mapping ++ [Cos line]
evalExpression line (Function "tan" rest) mapping = evalExpression line rest mapping ++ [Tan line]
evalExpression line (Function "asin" rest) mapping = evalExpression line rest mapping ++ [ArcSin line]
evalExpression line (Function "acos" rest) mapping = evalExpression line rest mapping ++ [ArcCos line]
evalExpression line (Function "atan" rest) mapping = evalExpression line rest mapping ++ [ArcTan line]
evalExpression line (Function "right$" (ExpressionList (e:e':[]))) mapping = 
    evalExpression line (ExpressionList (e:e':[])) mapping ++ [SubstrLast line]
evalExpression line (Function "left$" (ExpressionList (e:e':[]))) mapping = 
    evalExpression line e mapping ++ generatePush line (Integer' 1) mapping ++ evalExpression line e' mapping ++ [Substring line]
evalExpression line (Function "sqrt" rest) mapping = 
    evalExpression line rest mapping ++ generatePush line (Floating' 0.5) mapping ++ [Pow line]
evalExpression line e@(ExpressionList xs) mapping = evalExpressionList line e mapping
evalExpression line s@(Statement _ _) mapping = evalStatement line s mapping
evalExpression line (Constant i) mapping = generatePush line i mapping
evalExpression line v@(Variable i) mapping = generatePush line v mapping
evalExpression line a@(Array' _ _ _) mapping = generatePush line a mapping

evalIfThenStatement line (Constant (Integer' i)) mapping = 
    let jump = VIntegral $ renumberLine i mapping in
        [Push line (VStatement $ [Push line jump, Goto line])] ++ [IfThen line]
evalIfThenStatement line e mapping = [Push line $ VStatement $ evalExpression line e mapping] ++ [IfThen line]

evalLetStatement line (ExpressionList ((Variable s):xs)) mapping = 
    generatePush line s mapping ++ evalExpressionList line (ExpressionList xs) mapping ++ [Store line]
evalLetStatement line (ExpressionList (a@(Array' s i b):xs)) mapping = 
    generatePush line a mapping ++ evalExpressionList line (ExpressionList xs) mapping ++ [AStore line]

-- Assumes "on" statement was already extracted
evalOnGoStatement line (ExpressionList (e:(IntegerList is):(Statement g _):rest)) mapping =
    let is' = map (\(Constant (Integer' i)) -> renumberLine i mapping) is in
        evalExpression line e mapping ++ [Push line (VIntegerList is')] ++ 
        (if g == "goto" then [OnGoto line]
        else [Push line (VIntegral (line + 1)), PushCallstack line, OnGoto line])

evalDataStatement line index (ExpressionList []) mapping = []
evalDataStatement line index (ExpressionList (e:es)) mapping =
    generatePush line (String' "$stream") mapping ++ generatePush line (Integer' index) mapping ++ [ALoad line] ++ generatePush line e mapping ++ [AStore line] ++ -- Store the current element at index in $stream global array
    evalDataStatement line (index + 1) (ExpressionList es) mapping

increment line (Variable v@_) mapping = 
    generatePush line v mapping ++ generatePush line v mapping ++ [Load line] ++ generatePush line (Integer' 1) mapping ++ [Add line] ++ [Store line]
increment line v@(String' _) mapping = 
    generatePush line v mapping ++ generatePush line v mapping ++ [Load line] ++ generatePush line (Integer' 1) mapping ++ [Add line] ++ [Store line]
increment line v@(Array' _ _ _) mapping = 
    generatePush line v mapping ++ generatePush line v mapping ++ generatePush line (Integer' 1) mapping ++ [Add line] ++ [AStore line]

evalReadStatement line (Variable v@_) mapping =
    generatePush line v mapping ++ generatePush line (String' "$stream") mapping ++ generatePush line (String' "$streamidx") mapping ++ [Load line] ++ [ALoad line] ++ [Store line] ++ -- Store reference to current position in stream in variable v
    increment line (String' "$streamidx") mapping -- Increment $streamidx
evalReadStatement line a@(Array' _ _ _) mapping = -- Assume array
    generatePush line a mapping ++ generatePush line (String' "$stream") mapping ++ generatePush line (String' "$streamidx") mapping ++ [Load line] ++ [ALoad line] ++ [AStore line] ++ -- Store reference to current position in stream in variable v
    increment line (String' "$streamidx") mapping -- Increment $streamidx

-- evalStatement deals with things like if, for, goto, etc.
evalStatement line None mapping = []
evalStatement line (Statement "end" _) mapping = [End line]
evalStatement line (Statement "if" e) mapping = evalStatement line e mapping
evalStatement line (Statement "then" e) mapping = evalIfThenStatement line e mapping
evalStatement line (Statement "input" e) mapping = evalExpression line e mapping ++ [Input line]
evalStatement line (Statement "print" e) mapping = evalExpression line e mapping ++ [Print line]
evalStatement line (Statement "print!" e) mapping = evalExpression line e mapping ++ [PrintBang line]
evalStatement line (Statement "let" e) mapping = evalLetStatement line e mapping
evalStatement line (Statement "for" (ExpressionList ((Variable v):es))) mapping = generatePush line v mapping ++ evalExpressionList line (ExpressionList es) mapping
evalStatement line (Statement "tab" e) mapping = evalExpression line e mapping ++ [Spaces line]
evalStatement line (Statement "on" e) mapping = evalOnGoStatement line e mapping
evalStatement line (Statement "read" e) mapping = evalReadStatement line e mapping
evalStatement line (Statement "restore" _) mapping = generatePush line (String' "$streamidx") mapping ++ generatePush line (Integer' 1) mapping ++ [Store line] -- Reset $streamidx to 1
evalStatement line (Statement "data" e@(ExpressionList es)) mapping = 
    generatePush line (String' "$streamidx") mapping ++ generatePush line (Integer' 1) mapping ++ [Store line] ++ -- Generate the variable to store the current array index in
    generatePush line (String' "$stream") mapping ++ generatePush line (Integer' $ toInteger $ length es) mapping ++ [NewArray line] ++ -- Generate a new array to represent the stream - we are guaranteed to only have one per program due to preprocessing
    evalDataStatement line 1 e mapping
evalStatement line (Statement "dim" a@(Array' s i b)) mapping = 
    generateSimpleArrayPush line a mapping ++ [if i == 1 then NewArray line else NewArray2D line]
evalStatement line (Statement "to" (ExpressionList ((Variable (String' v)):e:es))) mapping =
    evalExpression line e mapping ++ [Store line] ++ [Push line (VString ("$" ++ v ++ "maxrange"))] ++ evalExpressionList line (ExpressionList es) mapping ++ [Store line] ++
    [Push line (VString ("$" ++ v ++ "jmp")), Push line (VIntegral $ line + 1), Store line] -- Stores the start of the for loop inside of (var name)+jmp
    --[Push line (VIntegral $ line + 1)] ++ [PushCallstack line]
evalStatement line (Statement "next" (Variable var@(String' v))) mapping =
    generatePush line var mapping ++ generatePush line var mapping ++ [Load line] ++ [Push line (VIntegral 1)] ++ [Add line] ++ [Store line] ++ -- This increments the variable
    generatePush line var mapping ++ [Load line] ++ [Push line (VString ("$" ++ v ++ "maxrange"))] ++ [Load line] ++ [LEqual line] ++ -- This performs the comparison to see if the loop is done
    [Push line $ VStatement $ [ -- Creating code which can jump to the top of the loop if necessary
        Push line (VString ("$" ++ v ++ "jmp")), Load line, Goto line -- Loads the jump location onto the stack and jumps
        ]] ++ [IfThen line]
evalStatement line (Statement "goto" (Integer' i)) mapping = 
    generatePush line (Integer' $ renumberLine i mapping) mapping ++ [Goto line]
evalStatement line (Statement "gosub" (Integer' i)) mapping = 
    let renumbered = renumberLine i mapping 
        nextLine = line + 1 in
        generatePush line (Integer' renumbered) mapping ++ generatePush line (Integer' nextLine) mapping ++
        [PushCallstack line] ++ [Goto line]
evalStatement line (Statement "return" _) mapping = [PopCallstack line] ++ [Goto line]
evalStatement line e@(ExpressionList _) mapping = evalExpressionList line e mapping
evalStatement line e@(Expression _ _) mapping = evalExpression line e mapping
evalStatement line _ mapping = []

--evalStatement :: Basic -> [(Integer, Integer)] -> [Bytecode]
evalStatementList _ None mapping = []
evalStatementList _ (StatementList []) mapping = []
evalStatementList line (StatementList (s:ss)) mapping = evalStatement line s mapping ++ 
                                                        evalStatementList line (StatementList ss) mapping

compile' :: [Basic] -> [(Integer, Integer)] -> [Bytecode]
compile' [] mapping = []
compile' (b@(Line l s):bs) mapping = evalStatementList l s mapping ++ compile' bs mapping

compile :: Sexpr -> [Bytecode]
compile s = case s of
    (Symbol s) -> []
    _ -> let (basic, mapping) = renumber (reorderBasic (translateSexpr s)) 0 0 [] in
        compile' basic mapping
