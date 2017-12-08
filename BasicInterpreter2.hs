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

foo = "(define foo " ++
    "'((100 input \"What is the value of A\" a )" ++
    " (110 input \"What is the value of B\" b )" ++
    " (120 input \"What is the value of C\" c )" ++
    " (130 let d = ((b * b) - (4.0 * (a * c))) )" ++
    " (140 print d) (150 end)))"

simpleArrayPrint = "(define foo '((100 dim w (10)) \
                   \(200 let w (2) = \"hello\") \
                   \(250 let w(3) = 1024) \
                   \(300 print w (2) : print w(3) : print w(4)))))"

testEverything = "(define test '((100 if 10 > 2 then print 10)" ++
                 "(150 gosub 190)" ++ 
                 "(160 print \"good job it gosubbed\")" ++
                 "(170 end)" ++
                 "(190 print \"skipped it\")" ++
                 "(200 return)" ++
                 "(500 end)))"

testRnd = "(define test '((100 print rnd(20))))"

guess = "(define guess '( (100 print tab(33) \"GUESS\" ) (110 print tab(15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (120 print ) (130 print \"THIS IS A NUMBER GUESSING GAME. I'LL THINK\" ) (140 print \"OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.\" ) (150 print \"THEN YOU HAVE TO GUESS WHAT IT IS.\" ) (160 print ) (170 input \"WHAT LIMIT DO YOU WANT\" l ) (180 print ) (190 let l1 = int (((log (l) / log (2)) + 1)) ) (200 print \"I'M THINKING OF A NUMBER BETWEEN 1 AND \" l ) (210 let g = 1 ) (220 let m = int (((l * rnd (1)) + 1)) ) (230 print ) (240 input \"WHAT IS YOUR GUESS\" n ) (250 print ) (260 if (n > 0) then 290 ) (270 print \"ILLEGAL VALUE.\" ) (280 goto 230 ) (290 if (n <= l) then 320 ) (300 print \"ILLEGAL VALUE.\" ) (310 goto 230 ) (320 if (n = m) then 390 ) (330 let g = (g + 1) ) (340 if (n > m) then 370 ) (350 print \"TOO LOW. TRY A BIGGER ANSWER.\" ) (360 goto 230 ) (370 print \"TOO HIGH. TRY A SMALLER ANSWER.\" ) (380 goto 230 ) (390 print \"THAT'S IT! YOU GOT IT IN \" g \" TRIES.\" ) (400 if (g < l1) then 440 ) (410 if (g = l1) then 450 ) (420 print \"YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY \" l1 \" TRIES.\" ) (430 end ) (440 print! \"VERY \" ) (450 print \"GOOD.\" ) (460 end )))"


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

{-
printEnvironment [] = putStr ""
printEnvironment (x:xs) = do
-}

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
    let (VString refName) = (vs !! (index - 1)) -- If this fails it is also invalid...
    (Just (_, ref)) <- findEnv refName -- Basically nothing in this function is allowed to fail
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

newArray' :: String -> Int -> [Value]
newArray' name 0 = []
newArray' name size = newArray' name (size - 1) ++ [VString $ "#" ++ name ++ show size]

-- Takes a name and a number and creates a 1-dimensional array
-- with that many elements
newArray = do
    (VString var, VIntegral size) <- binary
    let array' = newArray' var size
    forM_ array' $ \(VString s) -> do
        updateEnv s Null
        return (VString s)
    updateEnv var (VList array')

store :: VM Value
store = do
    (var, val) <- binary
    case var of
        (VString s) -> updateEnv s val
        (VSymbol s v) -> updateEnv s val

resolveStringType str =
    case dropWhile isDigit str of
        "" -> VIntegral (read str :: Int)
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
        (VIntegral i, VIntegral ii) -> return $ VBool $ op (fromIntegral i) (fromIntegral ii)
        (VIntegral i, VFloating f) -> return $ VBool $ op (fromIntegral i) f
        (VFloating f, VIntegral i) -> return $ VBool $ op f (fromIntegral i)
        (VFloating f, VFloating ff) -> return $ VBool $ op f ff

getStatement = do
  (x,ys) <- binary
  case (x,ys) of
    (VBool True, VStatement ys) -> do
      (program,env,rest,stack,callstack) <- get
      put(program,env,ys++rest,stack,callstack)
    (_,_) -> return ()

getSpaces = do
  (VIntegral num) <- unary
  return $ VString $ replicate num ' '

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

setLineNum = do
  (VIntegral line) <- unary
  (program,env,rest,stack,callstack) <- get
  let newRest = newProgram program line False
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
{-
getLineNum frame = let (frame', VIntegral line) = unary frame in (frame', line)
-}

arithmetic op = do
    (x, y) <- binary
    case (x, y) of
        (VIntegral i, VIntegral ii) -> return $ VIntegral $ round $ op (fromIntegral i) (fromIntegral ii)
        (VIntegral i, VFloating f) -> return $ VFloating $ op (fromIntegral i) f
        (VFloating f, VIntegral i) -> return $ VFloating $ op f (fromIntegral i)
        (VFloating f, VFloating ff) -> return $ VFloating $ op f ff

printStack [] = putStrLn ""
printStack (x:xs) = do
    --val <- resolveVarTypeValue x
    putStrLn (show x)
    printStack xs

vm' = do
    bytecode <- nextInstruction
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
        Or l -> do
            getOr >>= pushStack
            vm'
        And l -> do
            getAnd >>= pushStack
            vm'
        OnGoto l -> do
            setLineNum
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
        ALoad l -> do
            aload >>= pushStack
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