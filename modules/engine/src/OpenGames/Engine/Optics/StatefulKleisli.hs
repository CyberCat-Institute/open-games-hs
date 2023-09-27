{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.Optics.StatefulKleisli 
  ( StatefulKleisliOptic(..)
  , StatefulKleisliContext(..)
  ) where

import OpenGames.Engine.Optics
import OpenGames.Engine.Types

import           Control.Monad.State                hiding (state)
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)
import qualified Data.Vector as V
import           Numeric.Probability.Distribution   hiding (lift)

-------------------------------------------------------------
--- Stateful monadic optic


data StatefulKleisliOptic m s t a b where
    StatefulKleisliOptic :: (s -> m (z, a))
                         -> (z -> b -> StateT Vector m t)
                         -> StatefulKleisliOptic m s t a b

instance (Monad m) => Optic (StatefulKleisliOptic m) where
  lens v u = StatefulKleisliOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StatefulKleisliOptic v1 u1) (StatefulKleisliOptic v2 u2) = StatefulKleisliOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (StatefulKleisliOptic v1 u1) (StatefulKleisliOptic v2 u2) = StatefulKleisliOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (StatefulKleisliOptic v1 u1) (StatefulKleisliOptic v2 u2) = StatefulKleisliOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

data StatefulKleisliContext m s t a b where
  StatefulKleisliContext :: m (z, s) 
                         -> (z -> a -> StateT Vector m b) 
                         -> StatefulKleisliContext m s t a b

instance (Monad m) => Precontext (StatefulKleisliContext m) where
  void = StatefulKleisliContext (return ((), ())) (\() () -> return ())

instance (Monad m) => Context (StatefulKleisliContext m) (StatefulKleisliOptic m) where
  cmap (StatefulKleisliOptic v1 u1) (StatefulKleisliOptic v2 u2) (StatefulKleisliContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'}
               in StatefulKleisliContext h' k'
  (//) (StatefulKleisliOptic v u) (StatefulKleisliContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
               in StatefulKleisliContext h' k'
  (\\) (StatefulKleisliOptic v u) (StatefulKleisliContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
                  k' (z, s2) a1 = do {(_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1}
               in StatefulKleisliContext h' k'
