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


letters = "(define ltest '((100 let x = ((2 * 3) - 4))" ++
                          "(200 print x)" ++
                          "(300 if (x > 0) then 500)" ++
                          "(400 print 1000)" ++ 
                          "(500 print 10)))"

goCheck = "(define gotest '((100 print 5)" ++
                           "(200 print 10)" ++
                           "(300 goto 500)" ++
                           "(400 print 100000)" ++
                           "(500 print 1)" ++
                           "(600 print 3.14)))"

subCheck = "(define subtest '((100 print \"hi\")" ++
                            "(200 print 27)" ++
                            "(300 gosub 500)" ++
                            "(400 end)" ++
                            "(500 print 3.14)" ++
                            "(600 let z = 5)" ++
                            "(700 print z)" ++
                            "(800 print 1000)" ++
                            "(900 print 2000)" ++
                            "(1000 return)))"

subCheck2 = "(define subtestt '((100 print \"hi\")" ++
                            "(200 print 27)" ++
                            "(300 gosub 700)" ++
                            "(400 gosub 700)" ++
                            "(500 print 111)" ++
                            "(600 end)" ++
                            "(700 print 3.14)" ++
                            "(800 print \"Whats up\")" ++
                            "(900 return)))"

forCheck = "(define fortest '((100 for i = 1 to 10)" ++
                             "(150 for j = 1 to 10)" ++
                             "(200 print i)" ++
                             "(250 next j)" ++
                             "(300 next i)" ++
                             "(400 end)))"


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
  "(210 let s = 1 )"  ++
  "(220 for i = 1 to 10 )" ++
  "(230 let s = ((s + (d / s)) / 2.0) )" ++
  "(240 next i )" ++
  "(250 return )))"


guess = "(define guess '( (100 print tab(33) \"GUESS\" ) (110 print tab(15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (120 print ) (130 print \"THIS IS A NUMBER GUESSING GAME. I'LL THINK\" ) (140 print \"OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.\" ) (150 print \"THEN YOU HAVE TO GUESS WHAT IT IS.\" ) (160 print ) (170 input \"WHAT LIMIT DO YOU WANT\" l ) (180 print ) (190 let l1 = int (((log (l) / log (2)) + 1)) ) (200 print \"I'M THINKING OF A NUMBER BETWEEN 1 AND \" l ) (210 let g = 1 ) (220 let m = int (((l * rnd (1)) + 1)) ) (230 print ) (240 input \"WHAT IS YOUR GUESS\" n ) (250 print ) (260 if (n > 0) then 290 ) (270 print \"ILLEGAL VALUE.\" ) (280 goto 230 ) (290 if (n <= l) then 320 ) (300 print \"ILLEGAL VALUE.\" ) (310 goto 230 ) (320 if (n = m) then 390 ) (330 let g = (g + 1) ) (340 if (n > m) then 370 ) (350 print \"TOO LOW. TRY A BIGGER ANSWER.\" ) (360 goto 230 ) (370 print \"TOO HIGH. TRY A SMALLER ANSWER.\" ) (380 goto 230 ) (390 print \"THAT'S IT! YOU GOT IT IN \" g \" TRIES.\" ) (400 if (g < l1) then 440 ) (410 if (g = l1) then 450 ) (420 print \"YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY \" l1 \" TRIES.\" ) (430 end ) (440 print! \"VERY \" ) (450 print \"GOOD.\" ) (460 end )))"

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
print' :: Frame -> IO()
print' (Frame []) = putStrLn ""
print' (Frame (x:xs)) = do
    putStr (show x)
    print' (Frame xs)

printBang :: Frame -> IO()
printBang (Frame []) = putStr ""
printBang (Frame (x:xs)) = do
    putStr (show x)
    printBang (Frame xs)

--nextline is neecd for go sub
--same but adition of callstack
--callstack is another frame
--start as emput but popcallstacck,pushcallstck, then change our focus rto callstack frame
vm :: [Bytecode] -> Environment -> [Bytecode] -> Frame -> Frame -> IO ()
vm program env [] frame outFrame = putStr ""
vm program env ((End l):rest) frame outFrame = putStr ""
vm program env ((Push l a):rest) frame outFrame = vm program env rest (push frame a) outFrame
vm program env ((Input l):rest) frame outFrame = do
    (frame', env') <- input frame env
    vm program env' rest frame' outFrame
vm program env ((Load l):rest) frame outFrame = do
    let frame' = load frame env
    vm program env rest frame' outFrame
vm program env ((Store l):rest) frame outFrame = do
    let (frame', env') = store frame env
    vm program env' rest frame' outFrame
vm program env ((Add l):rest) frame outFrame = do
    let (frame', val) = arithmetic (+) frame
    vm program env rest (push frame' val) outFrame
vm program env ((Sub l):rest) frame outFrame = do
    let (frame', val) = arithmetic (-) frame
    vm program env rest (push frame' val) outFrame
vm program env ((Mult l):rest) frame outFrame = do
    let (frame', val) = arithmetic (*) frame
    vm program env rest (push frame' val) outFrame
vm program env ((Div l):rest) frame outFrame = do
    let (frame', val) = arithmetic (/) frame
    vm program env rest (push frame' val) outFrame
vm program env ((Equal l):rest) frame outFrame = do
    let (frame', val) = logical (==) frame
    vm program env rest (push frame' val) outFrame
vm program env ((NotEqual l):rest) frame outFrame = do
    let (frame', val) = logical (/=) frame
    vm program env rest (push frame' val) outFrame
vm program env ((Greater l):rest) frame outFrame= do
    let (frame', val) = logical (>) frame
    --print (frame', val)
    vm program env rest (push frame' val) outFrame
vm program env ((Less l):rest) frame outFrame = do
    let (frame', val) = logical (<) frame
    vm program env rest (push frame' val) outFrame
vm program env ((GEqual l):rest) frame outFrame = do
    let (frame', val) = logical (>=) frame
    vm program env rest (push frame' val) outFrame
vm program env ((LEqual l):rest) frame outFrame = do
    let (frame', val) = logical (<=) frame
    vm program env rest (push frame' val) outFrame
    
vm program env ((PushCallstack l):rest) frame outFrame = do
    let (frame', val) = unary frame
    vm program env rest frame' (push outFrame val)
vm program env ((PopCallstack l):rest) frame outFrame = do
    let (frame', val) = unary' outFrame
    vm program env rest (push frame val) frame' --(push frame val) frame'
vm program env ((IfThen l):rest) frame outFrame = do
    let (frame',res) = getStatement frame
    vm program env (res ++ rest) frame' outFrame
vm program env ((Goto l):rest) frame outFrame= do
  let (frame',num) = getLineNum frame
      newRest = newProgram program num False
  vm program env newRest frame' outFrame
vm program env ((PrintBang l):rest) frame outFrame= do
    printBang frame
    vm program env rest (Frame []) outFrame
vm program env ((Print l):rest) frame outFrame= do
    print' frame
    vm program env rest (Frame []) outFrame


--test frame = let (frame', x) = unary frame in (frame', x)

newProgram [] _ _ = []
newProgram (x:xs) l b = if (line x == l) || (b == True) then x:(newProgram xs l True) else newProgram xs l b

getLineNum frame = let (frame', VIntegral line) = unary frame in (frame', line)

getStatement frame = let (frame',((VBool x),(VStatement ys))) = binary frame in
                if x then (frame', ys)
                else (frame', [])
{-
stuff that doesnt work but might mess with

vm program env ((GoSub l):rest) frame = do
  let (frame',num) = getLineNum frame
      goSub = createGoSub program num
  vm program env (goSub ++ rest) frame'

{-
getIt frame = let (frame', (x, y)) = binary frame
                      in case (x, y) of
                           (VBool True,_) -> (frame', y)
                           (VBool False,_) -> (frame',x) -- []
-}
{-
--vm program env ((IfThen l):rest) frame = do
--  let (frame',result) = getLine env frame
--  vm program env rest (push frame' result)
-- (Double -> Double -> Bool) -> Frame -> (Frame, Value)
getLine env frame = let (frame', (x, y)) = binary frame
                      in case (x, y) of
                           (True,func) -> findEnv env "130"
                           (False,func) -> findEnv env "100"
-}
-}
