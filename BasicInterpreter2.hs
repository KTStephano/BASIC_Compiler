import Parselib
import Compiler
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import Control.Monad.IO.Class
import System.IO.Unsafe
import System.Random
import Data.IORef
import System.IO
import Control.Monad.Trans.State
import System.Environment

type Stack = [Value]
type CallStack = Stack
type Binding = (String, Value)
type Environment = [Binding]
type Program = [Bytecode]
type VMState = (Program, Environment, Program, Stack, CallStack)

type VM a = StateT VMState IO a

arr2Check = "(define arrtest '((100 dim a$ (2 5))" ++
                             "(130 a$ (1 1) = \"X\")" ++ 
                             "(150 print a$ (2 5))))"

arrCheck = "(define arrtest '((100 dim a$ (10))" ++
                             "(130 a$ (2) = \"X\")" ++
                             "(140 print a$ (3))" ++
                             "(150 print a$ (2))))"

ongoCheck = "(define test '((100 on 2 goto 110 120 130 140)" ++
                           "(110 print \"hey\")" ++
                           "(120 print \"you skipped hey\")" ++
                           "(130 let x = 10)" ++
                           "(140 print x)" ++
                           "(150 end)))"


hangman = "(define hangman '( (1000 print tab (32) \"HANGMAN\" ) (1010 print tab (15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (1020 print ) (1030 print ) (1040 print ) (1050 dim p$ (12 12) ) (1060 dim l$ (20) ) (1070 dim d$ (20) ) (1080 dim n$ (26) ) (1090 dim u (50) ) (1100 let c = 10 ) (1110 let n = 50 ) (1120 for i1 = 1 to 20 ) (1130 let d$ (i1) = \"-\" ) (1140 next i1 ) (1150 let m = 0 ) (1160 for i2 = 1 to 26 ) (1170 let n$ (i2) = \"\" ) (1180 next i2 ) (1190 for i3 = 1 to 12 ) (1200 for j1 = 1 to 12 ) (1210 let p$ (i3 j1) = \" \" ) (1220 next j1 ) (1230 next i3 ) (1240 for i4 = 1 to 12 ) (1250 let p$ (i4 1) = \"X\" ) (1260 next i4 ) (1270 for i5 = 1 to 7 ) (1280 let p$ (1 i5) = \"X\" ) (1290 next i5 ) (1300 let p$ (2 7) = \"X\" ) (1310 if (c < n) then 1340 ) (1320 print \"YOU DID ALL THE WORDS!!\" ) (1330 end ) (1340 let q = (int ((n * rnd (1))) + 1) ) (1350 if (u (q) = 1) then 1340 ) (1360 let u (q) = 1 ) (1370 let c = (c + 1) ) (1380 restore ) (1390 let t1 = 0 ) (1400 for i6 = 1 to q ) (1410 read a$ ) (1420 next i6 ) (1430 let l = len (a$) ) (1440 for i7 = 1 to len (a$) ) (1450 let l$ (i7) = mid$ (a$ i7 1) ) (1460 next i7 ) (1470 print \"HERE ARE THE LETTERS YOU USED:\" ) (1480 for i8 = 1 to 26 ) (1490 print! n$ (i8) ) (1500 if (n$ ((i8 + 1)) = \"\") then 1530 ) (1510 print! \",\" ) (1520 next i8 ) (1530 print ) (1540 print ) (1550 for i9 = 1 to l ) (1560 print! d$ (i9) ) (1570 next i9 ) (1580 print ) (1590 print ) (1600 input \"WHAT IS YOUR GUESS\" g$ ) (1610 let r = 0 ) (1620 for i10 = 1 to 26 ) (1630 if (n$ (i10) = \"\") then 1700 ) (1640 if (g$ <> n$ (i10)) then 1670 ) (1650 print \"YOU GUESSED THAT LETTER BEFORE!\" ) (1660 goto 1470 ) (1670 next i10 ) (1680 print \"PROGRAM ERROR. RUN AGAIN.\" ) (1690 end ) (1700 let n$ (i10) = g$ ) (1710 let t1 = (t1 + 1) ) (1720 for i11 = 1 to l ) (1730 if (l$ (i11) = g$) then 1770 ) (1740 next i11 ) (1750 if (r = 0) then 1800 ) (1760 goto 1820 ) (1770 let d$ (i11) = g$ ) (1780 let r = (r + 1) ) (1790 goto 1740 ) (1800 let m = (m + 1) ) (1810 goto 2050 ) (1820 for i12 = 1 to l ) (1830 if (d$ (i12) = \"-\") then 1860 ) (1840 next i12 ) (1850 goto 2030 ) (1860 print ) (1870 for i13 = 1 to l ) (1880 print! d$ (i13) ) (1890 next i13 ) (1900 print ) (1910 print ) (1920 input \"WHAT IS YOUR GUESS FOR THE WORD\" b$ ) (1930 if (a$ = b$) then 1970 ) (1940 print \"WRONG. TRY ANOTHER LETTER.\" ) (1950 print ) (1960 goto 1470 ) (1970 print \"RIGHT!! IT TOOK YOU \" t1 \" GUESSES!\" ) (1980 input \"WANT ANOTHER WORD\" w$ ) (1990 if (w$ = \"YES\") then 1120 ) (2000 print ) (2010 print \"IT'S BEEN FUN! BYE FOR NOW.\" ) (2020 goto 2920 ) (2030 print \"YOU FOUND THE WORD!\" ) (2040 goto 1980 ) (2050 print ) (2060 print ) (2070 print \"SORRY, THAT LETTER ISN'T IN THE WORD.\" ) (2080 on m goto 2090 2110 2130 2150 2170 2190 2210 2230 2250 2270 ) (2090 print \"FIRST, WE DRAW A HEAD\" ) (2100 goto 2280 ) (2110 print \"NOW WE DRAW A BODY.\" ) (2120 goto 2280 ) (2130 print \"NEXT WE DRAW AN ARM.\" ) (2140 goto 2280 ) (2150 print \"THIS TIME IT'S THE OTHER ARM.\" ) (2160 goto 2280 ) (2170 print \"NOW, LET'S DRAW THE RIGHT LEG.\" ) (2180 goto 2280 ) (2190 print \"THIS TIME WE DRAW THE LEFT LEG.\" ) (2200 goto 2280 ) (2210 print \"NOW WE PUT UP A HAND.\" ) (2220 goto 2280 ) (2230 print \"NEXT THE OTHER HAND.\" ) (2240 goto 2280 ) (2250 print \"NOW WE DRAW ONE FOOT\" ) (2260 goto 2280 ) (2270 print \"HERE'S THE OTHER FOOT -- YOU'RE HUNG!!\" ) (2280 on m goto 2290 2400 2440 2480 2530 2560 2590 2610 2630 2660 ) (2290 let p$ (3 6) = \"-\" ) (2300 let p$ (3 7) = \"-\" ) (2310 let p$ (3 8) = \"-\" ) (2320 let p$ (4 5) = \"(\" ) (2330 let p$ (4 6) = \".\" ) (2340 let p$ (4 8) = \".\" ) (2350 let p$ (4 9) = \")\" ) (2360 let p$ (5 6) = \"-\" ) (2370 let p$ (5 7) = \"-\" ) (2380 let p$ (5 8) = \"-\" ) (2390 goto 2680 ) (2400 for i14 = 6 to 9 ) (2410 let p$ (i14 7) = \"X\" ) (2420 next i14 ) (2430 goto 2680 ) (2440 for i15 = 4 to 7 ) (2450 let p$ (i15 (i15 - 1)) = \"\\\" ) (2460 next i15 ) (2470 goto 2680 ) (2480 let p$ (4 11) = \"/\" ) (2490 let p$ (5 10) = \"/\" ) (2500 let p$ (6 9) = \"/\" ) (2510 let p$ (7 8) = \"/\" ) (2520 goto 2680 ) (2530 let p$ (10 6) = \"/\" ) (2540 let p$ (11 5) = \"/\" ) (2550 goto 2680 ) (2560 let p$ (10 8) = \"\\\" ) (2570 let p$ (11 9) = \"\\\" ) (2580 goto 2680 ) (2590 let p$ (3 11) = \"\\\" ) (2600 goto 2680 ) (2610 let p$ (3 3) = \"/\" ) (2620 goto 2680 ) (2630 let p$ (12 10) = \"\\\" ) (2640 let p$ (12 11) = \"-\" ) (2650 goto 2680 ) (2660 let p$ (12 3) = \"-\" ) (2670 let p$ (12 4) = \"/\" ) (2680 for i16 = 1 to 12 ) (2690 for j2 = 1 to 12 ) (2700 print! p$ (i16 j2) ) (2710 next j2 ) (2720 print ) (2730 next i16 ) (2740 print ) (2750 print ) (2760 if (m <> 10) then 1470 ) (2770 print \"SORRY, YOU LOSE. THE WORD WAS \" a$ ) (2780 print! \"YOU MISSED THAT ONE. DO YOU \" ) (2790 goto 1980 ) (2800 input \"TYPE YES OR NO\" y$ ) (2810 if (left$ (y$ 1) = \"Y\") then 1120 ) (2820 data \"GUM\" \"SIN\" \"FOR\" \"CRY\" \"LUG\" \"BYE\" \"FLY\" ) (2830 data \"UGLY\" \"EACH\" \"FROM\" \"WORK\" \"TALK\" \"WITH\" \"SELF\" ) (2840 data \"PIZZA\" \"THING\" \"FEIGN\" \"FIEND\" \"ELBOW\" \"FAULT\" \"DIRTY\" ) (2850 data \"BUDGET\" \"SPIRIT\" \"QUAINT\" \"MAIDEN\" \"ESCORT\" \"PICKAX\" ) (2860 data \"EXAMPLE\" \"TENSION\" \"QUININE\" \"KIDNEY\" \"REPLICA\" \"SLEEPER\" ) (2870 data \"TRIANGLE\" \"KANGAROO\" \"MAHOGANY\" \"SERGEANT\" \"SEQUENCE\" ) (2880 data \"MOUSTACHE\" \"DANGEROUS\" \"SCIENTIST\" \"DIFFERENT\" \"QUIESCENT\" ) (2890 data \"MAGISTRATE\" \"ERRONEOUSLY\" \"LOUDSPEAKER\" \"PHYTOTOXIC\" ) (2900 data \"MATRIMONIAL\" \"PARASYMPATHOMIMETIC\" \"THIGMOTROPISM\" ) (2910 print \"BYE NOW\" ) (2920 end )))"

nextInstruction :: VM Bytecode
nextInstruction = StateT $ \(program, env, rest, stack, callstack) ->
    case rest of
        [] -> return (End 0, (program, env, rest, stack, callstack))
        (bytecode:rest') -> return (bytecode, (program, env, rest', stack, callstack))

pushStack :: Value -> VM Value
pushStack v = StateT $ \(program, env, rest, stack, callstack) -> 
    do
        return (v, (program, env, rest, stack ++ [v], callstack))

pushCallStack :: Value -> VM Value
pushCallStack v = StateT $ \(program, env, rest, stack, callstack) ->
    do
        return (v, (program, env, rest, stack, callstack ++ [v]))

popStack :: VM Value
popStack = StateT $ \e@(program, env, rest, stack, callstack) ->
    do
        if stack == [] then return (Null, e)
        else do
            let rst = reverse stack
            let x = head rst
            let rst' = reverse $ drop 1 rst
            return (x, (program, env, rest, rst', callstack))

popCallStack :: VM Value
popCallStack = StateT $ \e@(program, env, rest, stack, callstack) ->
    do
        if callstack == [] then return (Null, e)
        else do
            -- reverse callstack
            let rst = reverse callstack
            -- pull out the head of the reversed stack
            let x = head rst
            let rst' = reverse $ drop 1 rst
            return (x, (program, env, rest, stack, rst'))

findEnv :: String -> VM (Maybe Binding)
findEnv str = StateT $ \(program, env, rest, stack, callstack) ->
    do
        -- Filter out everything except the entry we care about
        let result = filter (\(s, v) -> str == s) env
        case result of
            [] -> return (Nothing, (program, env, rest, stack, callstack))
            (x:xs) -> return (Just x, (program, env, rest, stack, callstack))

newEnvValue = do
    v <- newIORef Null
    return $ VDataRef v

{-
updateEnv :: String -> Value -> VM Value
updateEnv str value = StateT $ \env ->
    do
        f <- newEnvValue
        return (f, env)
-}

updateEnv :: String -> Value -> VM Value
updateEnv str value = do
    binding <- findEnv str
    case binding of
        Nothing -> do
            v@(VDataRef var) <- (liftIO newEnvValue)
            (program, env, rest, stack, callstack) <- get
            liftIO (writeIORef var value)
            put (program, env ++ [(str, v)], rest, stack, callstack)
            return v
        Just (s, v@(VDataRef var)) -> do
            liftIO (writeIORef var value)
            return v


-- Pops a value off the stack and returns it unchanged
unary' :: VM Value
unary' = popStack

-- Pops a value off the stack. However, if the value is a VDataRef,
-- it will resolve the actual value of the reference. Along with this,
-- if the value is a VSymbole (s, v), it will return v (and dereference v if necessary)
unary :: VM Value
unary = do
    result <- unary'
    case result of
        (VSymbol s v) -> do
            varType <- liftIO (resolveVarTypeValue v)
            return varType
        v@_ -> do
            varType <- liftIO (resolveVarTypeValue v)
            return varType

-- Works the same as unary', but for two values
binary' :: VM (Value, Value)
binary' = do
    y <- unary'
    x <- unary'
    return (x, y)

-- Works the same as unary, but for two values
binary :: VM (Value, Value)
binary = do
    y <- unary
    x <- unary
    return (x, y)

ternary' :: VM (Value, Value, Value)
ternary' = do
    z <- unary'
    y <- unary'
    x <- unary'
    return (x, y, z)

ternary :: VM (Value, Value, Value)
ternary = do
    z <- unary
    y <- unary
    x <- unary
    return (x, y, z)

load :: VM Value
load = do
    (VString var) <- unary
    val <- findEnv var
    case val of
        Nothing -> do
            val' <- updateEnv var Null
            let result = VSymbol var val'
            return result
        Just (s, v) -> do
            let result = VSymbol s v
            return result

-- Loads a reference from a 1-dimensional array onto the stack
-- At the time of the call, the stack looks like:
--      [ var_name,
--        index ]
-- After the call, the stack looks like:
--      [ var_reference ]
aload :: VM Value
aload = do
    (VString var, VIntegral index) <- binary
    (Just (varName, array')) <- findEnv var -- If this ever fails it is a critical error and should crash the program
    (VList vs) <- (liftIO $ resolveVarTypeValue array') -- This also should not fail
    let (VString refName) = (vs !! (fromIntegral $ index - 1)) -- If this fails it is also invalid...
    (Just (_, ref)) <- findEnv refName -- Basically nothing in this function is allowed to fail
    return ref

aLoad2D = do
  (VString var, VIntegral w, VIntegral h) <- ternary
  (Just (varName, array')) <- findEnv var
  (VList vs) <- (liftIO $ resolveVarTypeValue array')
  let (VList bs) = vs !! (fromIntegral $ h - 1) 
      (VString refName) = (bs !! (fromIntegral $ w - 1))
  (Just (_,ref)) <- findEnv refName
  return ref

-- Stores a value into a reference from an array. At the time of the
-- call, the stack looks like:
--      [ var_reference 
--        value ]
-- And after the call the stack is empty.
astore :: VM ()
astore = do
    val <- unary
    (VDataRef ref) <- unary' -- Make sure to use unary' because unary dereferences things
    liftIO (writeIORef ref val)

newArray' :: String -> Integer -> [Value]
newArray' name 0 = []
newArray' name size = newArray' name (size - 1) ++ [VString $ "#" ++ name ++ show size]

-- Takes a name and a number and creates a 1-dimensional array
-- with that many elements
newArray = do
    (VString var, VIntegral size) <- binary
    let array' = newArray' var size
    forM_ array' $ \(VString s) -> do
        updateEnv s (VIntegral 0)
        return (VString s)
    updateEnv var (VList array')

help2D name 0 _ = []
help2D name size index = help2D name (size - 1) index ++  [VString $ "#" ++ name ++ show (size -1) ++ "," ++ show index]

newArray2D' name width height =  VList [VList $ help2D name width h | h <- [0..(height-1)]]

newArray2D = do
  (VString var, VIntegral width, VIntegral height) <- ternary

  let (VList array') =  newArray2D' var width height
  forM_ array' $ \(VList l) -> do
     innerArray var l
  updateEnv var (VList array')
      
{-
      forM_ array $ \(VString s) -> do
        updateEnv s Null
        return (VString s)
      updateEnv var (VList array')
-}    
innerArray var array = do
  forM_ array $ \(VString s) -> do
    updateEnv s (VIntegral 0)
    return (VString s)
  --updateEnv var (VList array)

  
store :: VM Value
store = do
    (var, val) <- binary
    case var of
        (VString s) -> updateEnv s val
        (VSymbol s v) -> updateEnv s val

resolveStringType str =
    case dropWhile isDigit str of
        "" -> VIntegral (read str :: Integer)
        ('.':xs) -> case dropWhile isDigit xs of
            "" -> VFloating (read str :: Double)
            _ -> VString str
        _ -> VString str

input :: VM Value
input = do
    (VString str, (VSymbol var val)) <- binary'
    liftIO $ putStrLn ((filter (\p -> '"' /= p) str) ++ "?")
    s <- liftIO $ getLine
    updateEnv var (resolveStringType s)

-- If the type of the given Value is VDataRef, this will extract the actual
-- value (dereference), otherwise it will just return the input unchanged
resolveVarTypeValue value = 
    case value of
        (VDataRef var) -> do
            readIORef var
        _ -> return value

print' = do
    printBang
    liftIO $ putStrLn ""
{-
printBang = do
    (program, env, rest, stack, callstack) <- get
    forM stack $ \s -> do
        case s of
            VString s' -> liftIO $ putStr (filter (\p -> '"' /= p) s')
            _ -> do
                f <- liftIO $ resolveVarTypeValue s
                liftIO $ putStr (show f)
    put (program, env, rest, [], callstack)
-}

printBang = do
    val <- unary
    case val of
        Null -> liftIO $ putStr ""
        _ -> do
            printBang
            f <- liftIO $ resolveVarTypeValue val
            case f of
                VString s -> liftIO $ putStr (filter (\p -> '"' /= p) s)
                _ -> liftIO $ putStr (show f)


logical op = do
    (x, y) <- binary
    case (x, y) of
        (VDataRef ref, VDataRef ref') -> do
            x' <- liftIO $ resolveVarTypeValue x
            y' <- liftIO $ resolveVarTypeValue y
            return $ VBool $ op x' y'
        (VDataRef ref, _) -> do
            x' <- liftIO $ resolveVarTypeValue x
            return $ VBool $ op x' y
        (_, VDataRef ref) -> do
            y' <- liftIO $ resolveVarTypeValue y
            return $ VBool $ op x y'
        (_, _) -> return $ VBool $ op x y

getStatement = do
  (x,ys) <- binary
  case (x,ys) of
    (VBool True, VStatement ys) -> do
      (program,env,rest,stack,callstack) <- get
      put(program,env,ys++rest,stack,callstack)
    (_,_) -> return ()

getSpaces = do
  (VIntegral num) <- unary
  return $ VString $ replicate (fromIntegral $ num) ' '

getFloor = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VIntegral x
    (VFloating x) -> return $ VIntegral $ floor x
    
getAbs = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VIntegral $ abs x
    (VFloating x) -> return $ VFloating $ abs x

getLog = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ (log (fromIntegral x))
    (VFloating x) -> return $ VFloating $ log x

getPower = do
  (x,y) <- binary
  case (x,y) of
    (VIntegral i, VIntegral ii) -> return $ VIntegral $ i ^ ii
    (VFloating f, VIntegral i) -> return $ VFloating $ f ** (fromIntegral i)
    (VIntegral i, VFloating f) -> return $ VFloating $ (fromIntegral i) ** f
    (VFloating f, VFloating ff) -> return $ VFloating $ f ** ff

getRand = do
  (VIntegral num) <- unary
  rand <- liftIO $ (randomRIO(0,(fromIntegral num)) :: IO Double)
  return $ VFloating  rand ---- $ VFloating $ unsafePerformIO rand

{-
getSin = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ sin(x)
    (VFloating x) -> return $ VFloating $ sin(x)

getArcSin = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ asin(x)
    (VFloating x) -> return $ VFloating $ asin(x)

getCos = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ cos(x)
    (VFloating x) -> return $ VFloating $ cos(x)

getArcCos = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ acos(x)
    (VFloating x) -> return $ VFloating $ acos(x)

getTan = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ tan(x)
    (VFloating x) -> return $ VFloating $ tan(x)

getArctan = do
  x <- unary
  case x of
    (VIntegral x) -> return $ VFloating $ atan(x)
    (VFloating x) -> return $ VFloating $ atan(x)
-}

getLength = do
  (VString str) <- unary
  return $ VIntegral $ toInteger $ (length str)

--string
--starting position of the substring
--length of the substring

--hello
--  ll

getSubstring = do
  (VString str, VIntegral start, VIntegral length) <- ternary
  let x = accSubstring str start length 0 1
  liftIO $ print x
  return $ VString $ x

accSubstring [] _ _ _ _ = []
accSubstring (s:str) start goal cur pos = if pos < start then accSubstring str start goal cur (pos + 1)
                                          else if cur < goal then s:(accSubstring str start goal (cur + 1) (pos + 1))
                                          else []

setLineNum = do
  (VIntegral line) <- unary
  (program,env,rest,stack,callstack) <- get
  let newRest = newProgram program line False
  put(program,env,newRest,stack,callstack)

setListLineNum = do
  (VIntegral line, VIntegerList list) <- binary
  (program,env,rest,stack,callstack) <- get
  let newRest = newProgram program (list !! (fromIntegral $ line - 1)) False
  put(program,env,newRest,stack,callstack)
  
getOr = do
  (x,y) <- binary
  case (x,y) of
    (VBool True,_) -> return $ VBool True
    (_,VBool True) -> return $ VBool True
    (_,_) -> return $ VBool False

getAnd = do
  (x,y) <- binary
  case (x,y) of
    (VBool True, VBool True) -> return $ VBool True
    (_,_) -> return $ VBool False

newProgram [] _ _ = []
newProgram (x:xs) l b = if (line x == l) || (b == True) then x:(newProgram xs l True) else newProgram xs l b

arithmetic op = do
    (x, y) <- binary
    case (x, y) of
        (VDataRef ref, VDataRef ref') -> do
            x' <- liftIO $ resolveVarTypeValue x
            y' <- liftIO $ resolveVarTypeValue y
            return $ op x' y'
        (VDataRef ref, _) -> do
            x' <- liftIO $ resolveVarTypeValue x
            return $ op x' y
        (_, VDataRef ref) -> do
            y' <- liftIO $ resolveVarTypeValue y
            return $ op x y'
        (_, _) -> return $ op x y

printStack [] = putStrLn ""
printStack (x:xs) = do
    --val <- resolveVarTypeValue x
    putStrLn (show x)
    printStack xs

vm' = do
    bytecode <- nextInstruction
    --liftIO $ print bytecode
    case bytecode of
        End l -> do
            liftIO $ putStr ""
        Push l a -> do
            pushStack a
            vm'
        Print l -> do
            print'
            vm'
        PrintBang l -> do
            printBang
            vm'
        Input l -> do
            input
            vm'
        Load l -> do
            load >>= pushStack
            vm'
        Store l -> do
            store
            vm'
        Add l -> do
            arithmetic (+) >>= pushStack
            vm'
        Sub l -> do
            arithmetic (-) >>= pushStack
            vm'
        Mult l -> do
            arithmetic (*) >>= pushStack
            vm'
        Div l -> do
            arithmetic (/) >>= pushStack
            vm'
        Equal l -> do
            logical (==) >>= pushStack
            vm'
        NotEqual l -> do
            logical (/=) >>= pushStack
            vm'
        Greater l -> do
            logical (>) >>= pushStack
            vm'
        GEqual l -> do
            logical (>=) >>= pushStack
            vm'
        Less l -> do
            logical (<) >>= pushStack
            vm'
        LEqual l -> do
            logical (<=) >>= pushStack
            vm'
        Spaces l -> do
            getSpaces >>= pushStack
            vm'
        CastInt l -> do
            getFloor >>= pushStack
            vm'
        Abs l -> do
            getAbs >>= pushStack
            vm'
        Log l -> do
            getAbs >>= pushStack
            vm'
        Pow l -> do
            getPower >>= pushStack
            vm'
        Rand l -> do
            getRand >>= pushStack
            vm'
        --Sin l -> do
        --ArcSin l -> do
        --Cos l -> do
        --ArcCos l -> do
        --Tan l -> do
        --ArcTan l -> do
        Or l -> do
            getOr >>= pushStack
            vm'
        And l -> do
            getAnd >>= pushStack
            vm'
        OnGoto l -> do
            setListLineNum
            vm'
        PushCallstack l -> do
            unary >>= pushCallStack
            vm'
        PopCallstack l -> do
            popCallStack >>= pushStack
            vm'
        IfThen l -> do
            getStatement
            vm'
        Goto l -> do
            setLineNum
            vm'
        NewArray l -> do
            newArray
            vm'
        NewArray2D l -> do
            newArray2D
            vm'
        ALoad l -> do
            aload >>= pushStack
            vm'
        ALoad2D l -> do
            aLoad2D >>= pushStack
            vm'
        Length l -> do
            getLength >>= pushStack
            vm'
        Substring l -> do
            getSubstring >>= pushStack
            vm'
        AStore l -> do
            astore
            vm'

vm program = do
    (out, _) <- runStateT vm' (program, [], program, [], [])
    return out

main :: IO ()
main = do
    args <- getArgs
    if (length args == 0) then
        putStrLn "No file input"
    else do
        handle <- openFile (args !! 0) ReadMode
        contents <- hGetContents handle
        let parsed = analyze (filter (\s -> s /= '\n' && s /= '\r') contents)
        case parsed of
            (Symbol _) -> putStrLn "Error parsing input"
            _ -> do
                let bytecode = compile parsed
                vm bytecode
