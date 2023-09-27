{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.Optics.StochasticStateful 
  ( Stochastic(..)
  , Vector(..)
  , StochasticStatefulOptic(..)
  , StochasticStatefulContext(..)
  ) where

import OpenGames.Engine.Optics
import OpenGames.Engine.Types

import           Control.Monad.State                hiding (state)
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)
import qualified Data.Vector as V
import           Numeric.Probability.Distribution   hiding (lift)

-------------------------------------------------------------
--- replicate the old implementation of a stochastic context
type Stochastic = T Double


data StochasticStatefulOptic s t a b where
  StochasticStatefulOptic :: (s -> Stochastic (z, a))
                          -> (z -> b -> StateT Vector Stochastic t)
                          -> StochasticStatefulOptic s t a b

instance Optic StochasticStatefulOptic where
  lens v u = StochasticStatefulOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

data StochasticStatefulContext s t a b where
  StochasticStatefulContext :: (Show z) => Stochastic (z, s) -> (z -> a -> StateT Vector Stochastic b) -> StochasticStatefulContext s t a b

instance Precontext StochasticStatefulContext where
  void = StochasticStatefulContext (return ((), ())) (\() () -> return ())

instance Context StochasticStatefulContext StochasticStatefulOptic where
  cmap (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) (StochasticStatefulContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'}
               in StochasticStatefulContext h' k'
  (//) (StochasticStatefulOptic v u) (StochasticStatefulContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
               in StochasticStatefulContext h' k'
  (\\) (StochasticStatefulOptic v u) (StochasticStatefulContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
                  k' (z, s2) a1 = do {(_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1}
               in StochasticStatefulContext h' k'

instance ContextAdd StochasticStatefulContext where
  prl (StochasticStatefulContext h k)
    = let fs = [((z, s1), p) | ((z, Left s1), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a1 -> k z (Left a1)))
  prr (StochasticStatefulContext h k)
    = let fs = [((z, s2), p) | ((z, Right s2), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a2 -> k z (Right a2)))

