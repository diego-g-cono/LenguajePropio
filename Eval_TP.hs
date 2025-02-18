module Eval_TP (eval) where

import AST_TP
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Concurrent  (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Estados
type Env = [(Variable,Integer)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado-error-tick
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Env, Integer) }

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> Just (x, s, 0))
    m >>= f = StateErrorTick (\s -> do (v, s', t) <- runStateErrorTick m s
                                       (v', s'', t') <- runStateErrorTick (f v) s'
                                       return (v', s'', t + t'))
--    m >>= f = StateErrorTick (\s -> case runStateErrorTick m s of
--                                      Nothing -> Nothing
--                                      Just (v, s', t) -> case runStateErrorTick (f v) s' of
--                                                           Nothing -> Nothing
--                                                           Just (v', s'', t') -> Just (v', s'', t + t'))

-- Clase para representar mónadas con estado de variables.
class Monad m => MonadState m where
    lookfor :: Variable -> m Integer
    update :: Variable -> Integer -> m ()

instance MonadState StateErrorTick where
    lookfor var = StateErrorTick (\s -> maybe Nothing (\v -> Just (v, s, 0)) (lookfor' var s))
                  where lookfor' var []               = Nothing
                        lookfor' var ((var', val):ss) | var == var' = Just val
                                                      | otherwise   = lookfor' var ss
    update var val = StateErrorTick (\s -> Just ((), update' var val s, 0))
                     where update' var val [] = [(var, val)]
                           update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                             | otherwise   = (var', val'):(update' var val ss)

-- Clase para representar mónadas que lanzan errores.
class Monad m => MonadError m where
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> Nothing)

-- Clase para representar mónadas que cuentan operaciones.
class Monad m => MonadTick m where
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> Just ((), s, 1))

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM

instance Applicative StateErrorTick where
    pure = return
    (<*>) = ap

-- Evalua un programa en el estado nulo
eval :: Comm -> (Env, Integer)
eval p = case runStateErrorTick (evalComm p) initState of
    Just (v, s, t) -> (s, t) 
    Nothing        -> error "ERROR!"

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let v e)      = do val <- evalIntExp e
                             update v val
evalComm (Seq l r)      = do evalComm l
                             evalComm r
evalComm (Cond b tc fc) = do bval <- evalBoolExp b
                             if bval then evalComm tc
                             else evalComm fc
evalComm (Repeat c b)    = do bval <- evalBoolExp b
                              if bval then evalComm (Seq c (Repeat c b))
                              else return ()
                              
evalComm (Switch e [] defaultCase) = evalComm defaultCase
evalComm (Switch e (c:cs) defaultCase) = do val <- evalIntExp e
                                            if val == fst c then evalComm (snd c) 
                                            else evalComm (Switch e cs defaultCase)

evalComm (Ternario b tc fc) = do bval <- evalBoolExp b
                                 if bval then evalComm tc
                                 else evalComm fc


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Integer
evalIntExp (Const n)   = return n
evalIntExp (Var v)     = do val <- lookfor v
                            return val
evalIntExp (UMinus e)  = do val <- evalIntExp e
                            return (negate val)
evalIntExp (Plus l r)  = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval + rval)
evalIntExp (Minus l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval - rval)
evalIntExp (Times l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval * rval)
evalIntExp (Div l r)   = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw
                            else do tick
                                    return (div lval rval)
evalIntExp (Promedio es) = do vals <- mapM evalIntExp es
                              return (sum vals `div`  count vals)

evalIntExp (LinExp cs n) = do mapM evalComm cs
                              x <- evalIntExp n
                              return (x)

--Funcion auxiliar para el promedio                              
count::[Integer] -> Integer
count [] = 0
count [x] = 1
count (x:xs) = 1 + count xs

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval == rval)
evalBoolExp (Lt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval < rval)
evalBoolExp (Gt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval && rval)
evalBoolExp (Or l r)  = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval || rval)
evalBoolExp (Not b)   = do bval <- evalBoolExp b
                           return (not bval)
