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

type Stack = [Value]
type CallStack = Stack
type Binding = (String, Value)
type Environment = [Binding]
type Program = [Bytecode]
type VMState = (Program, Environment, Program, Stack, CallStack)

type VM a = StateT VMState IO a

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
            let rst' = filter (\e -> e /= x) stack
            return (x, (program, env, rest, rst', callstack))

popCallStack :: VM Value
popCallStack = StateT $ \e@(program, env, rest, stack, callstack) ->
    do
        if stack == [] then return (Null, e)
        else do
            -- reverse callstack
            let rst = reverse callstack
            -- pull out the head of the reversed stack
            let x = head rst
            let rst' = filter (\e -> e /= x) callstack
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

unary' :: VM Value
unary' = popStack

unary :: VM Value
unary = do
    result <- unary'
    case result of
        (VSymbol s v) -> return v
        _ -> return result

binary' :: VM (Value, Value)
binary' = do
    y <- unary'
    x <- unary'
    return (x, y)

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

resolveVarTypeValue value = 
    case value of
        (VDataRef var) -> do
            readIORef var
        _ -> return value

print' = do
    printBang
    liftIO $ putStrLn ""

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
            val <- load
            pushStack val
            vm'

vm program = do
    (out, _) <- runStateT vm' (program, [], program, [], [])
    return out