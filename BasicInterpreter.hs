-- * = 0 or mroe
-- + = 1 or more
-- {} = symbolic class or something
-- lower = lowercase digit
-- digit = number

import Parselib
import Compiler
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe

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


newtype Environment = Environment {getEnv :: [(String, Value)]} deriving (Show, Eq)

findEnv :: Environment -> String -> [(String, Value)]
findEnv (Environment []) _ = []
findEnv (Environment ((s,v):xs)) str = if s == str then [(s,v)] else findEnv (Environment xs) str

updateEnv :: Environment -> String -> Value -> Environment
updateEnv env str val = let x = findEnv env str
                        in if (x == []) then Environment $ (getEnv env) ++ [(str, val)]
                           else Environment $ (filter (\y -> y /= (x !! 0)) $ getEnv env) ++ [(str, val)]

data Frame = Frame {getStack :: [Value]} deriving (Show)

getStackValue (Frame xs) index = xs !! index

getStackLength (Frame xs) = length xs

load :: Frame -> Environment -> Frame
load frame env = let (frame', (VString var)) = pop frame
                     val = findEnv env var
                 in case val of
                    [] -> push frame' (VSymbol var Null)
                    (x:xs) -> push frame' (VSymbol (fst x) (snd x))

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

unary' frame = pop frame

unary frame = let (frame', val) = unary' frame
              in case val of
                (VSymbol s v) -> (frame', v)
                s -> (frame', s)

binary frame = let (frame', y) = unary frame
                   (frame'', x) = unary frame'
               in (frame'', (x, y))

binary' frame = let (frame', y) = unary' frame
                    (frame'', x) = unary' frame'
                in (frame'', (x, y))

logical op frame = let (frame', (x, y)) = binary frame
                   in case (x, y) of
                     ((VIntegral i), (VIntegral ii)) -> (frame', VBool $ op (fromIntegral i) (fromIntegral ii))
                     ((VIntegral i), (VFloating ii)) -> (frame', VBool $ op (fromIntegral i) ii)
                     ((VFloating i), (VIntegral ii)) -> (frame', VBool $ op i (fromIntegral ii))
                     ((VFloating i), (VFloating ii)) -> (frame', VBool $ op i ii)

arithmetic op frame = let (frame', (x, y)) = binary frame
                      in case (x, y) of
                        ((VIntegral i, VIntegral ii)) -> (frame', VIntegral $ round $ op (fromIntegral i) (fromIntegral ii))
                        ((VFloating i, VFloating ii)) -> (frame', VFloating $ op i ii)
                        ((VIntegral i), (VFloating ii)) -> (frame', VFloating $ op (fromIntegral i) ii)
                        ((VFloating i), (VIntegral ii)) -> (frame', VFloating $ op i (fromIntegral ii))
                        ((VIntegral i), (VString s)) -> (frame', VFloating $ op (fromIntegral i) (read s :: Double))
                        ((VString s), (VIntegral i)) -> (frame', VFloating $ op (read s :: Double) (fromIntegral i))
                        ((VFloating i), (VString s)) -> (frame', VFloating $ op i (read s :: Double))
                        ((VString s), (VFloating i)) -> (frame', VFloating $ op (read s :: Double) i)
                        ((VString s), (VString s')) -> (frame', VFloating $ op (read s :: Double) (read s' :: Double))

{-
add (VIntegral i) (VIntegral j) = VIntegral $ i + j
add (VFloating i) (VFloating j) = VFloating $ i + j

sub (VIntegral i) (VIntegral j) = VIntegral $ i - j
sub (VFloating i) (VFloating j) = VFloating $ i - j

mult (VIntegral i) (VIntegral j) = VIntegral $ i * j
mult (VFloating i) (VFloating j) = VFloating $ i * j
-}

input frame env = let (frame', (VSymbol var _)) = pop frame
                      (frame'', (VString str)) = pop frame'
                  in do
                        putStrLn (str ++ "?")
                        s <- getLine
                        let env' = updateEnv env var (VString s)
                        return (frame'', env')


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
vm program env ((Push l a):rest) frame = vm program env rest (push frame a)
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
    let (frame', val) = arithmetic (+) frame
    vm program env rest (push frame' val)
vm program env ((Sub l):rest) frame = do
    let (frame', val) = arithmetic (-) frame
    vm program env rest (push frame' val)
vm program env ((Mult l):rest) frame = do
    let (frame', val) = arithmetic (*) frame
    vm program env rest (push frame' val)
vm program env ((Div l):rest) frame = do
    let (frame', val) = arithmetic (/) frame
    vm program env rest (push frame' val)
vm program env ((Equal l):rest) frame = do
    let (frame', val) = logical (==) frame
    vm program env rest (push frame' val)
vm program env ((NotEqual l):rest) frame = do
    let (frame', val) = logical (/=) frame
    vm program env rest (push frame' val)
vm program env ((Greater l):rest) frame = do
    let (frame', val) = logical (>) frame
    vm program env rest (push frame' val)
vm program env ((Less l):rest) frame = do
    let (frame', val) = logical (<) frame
    vm program env rest (push frame' val)
vm program env ((GEqual l):rest) frame = do
    let (frame', val) = logical (>=) frame
    vm program env rest (push frame' val)
vm program env ((LEqual l):rest) frame = do
    let (frame', val) = logical (<=) frame
    vm program env rest (push frame' val)
vm program env ((PrintBang l):rest) frame = do
    printBang frame
    vm program env rest (Frame [])
vm program env ((Print l):rest) frame = do
    print' frame
    vm program env rest (Frame [])