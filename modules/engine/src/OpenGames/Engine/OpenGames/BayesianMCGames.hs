{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module OpenGames.Engine.OpenGames.BayesianMCGames
  ( DiagnosticInfoMC(..)
  , decisionMC
  , fromLens
  , fromFunctions
  , nature
  ) where


import Control.Arrow                      hiding ((+:+), (+++))
import Control.Monad.State                hiding (state)
import Control.Monad.Trans.Class

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Strict

import Data.Foldable
import Data.HashMap                       as HM hiding (null,map,mapMaybe)
import Data.Hashable

import Data.List (maximumBy, nub, sort)
import Data.Ord (comparing)

import OpenGames.Engine.OpenGames hiding (lift)
import OpenGames.Engine.OpenGames.Utils
import OpenGames.Engine.Optics
import OpenGames.Engine.Optics.StatefulKleisli
import OpenGames.Engine.TLL
import OpenGames.Engine.Types

{-
Stateful bayesian games using monte carlo
-}

-- Defining the necessary types for outputting information of a BayesianGame with monte carlo
data DiagnosticInfoMC x y = DiagnosticInfoMC {
    equilibrium :: Bool
  , player :: String
  , optimalMove :: y
  , strategy :: MCIO y
  , optimalPayoff :: Double
  , context :: y -> MCIO Double
  , payoff :: Double
  , state :: x
  -- , unobservedState :: ?
}


type StatefulKleisliOpenGame m a b x s y r 
  = OpenGame (StatefulKleisliOptic m) (StatefulKleisliContext m) a b x s y r

instance Show (DiagnosticInfoMC x y) where
  show DiagnosticInfoMC{..} = "equilibrium \n" ++ (show equilibrium) ++ "\n player \n" ++ (show player) ++ "\n optimalPayoff \n" ++ (show optimalPayoff) ++ "\n payoff \n" ++ (show payoff)

type MCIO = Population SamplerIO

mcSupport :: (Eq a, Ord a) => Int -> MCIO a -> IO [a]
mcSupport numParticles a = do xs <- sampleIO 
                                  $ fmap (Prelude.map fst) 
                                  $ explicitPopulation 
                                  $ withParticles numParticles a
                              return (sort $ nub xs)

mcExpectation :: Int -> MCIO Double -> IO Double
mcExpectation numParticles = sampleIO . popAvg id . withParticles numParticles

decisionMC :: (Eq x, Ord x) 
           => {- numParticles: -} Int
           -> {- epsilon: -} Double
           -> {- name: -} String
           -> {- options: -} (x -> [y])
           -> StatefulKleisliOpenGame MCIO
                                      '[Kleisli MCIO x y] '[IO [DiagnosticInfoMC x y]] 
                                      x () y Payoff
decisionMC numParticles epsilon name options = OpenGame {
  play = \(a ::- Nil) -> let v x = do {y <- runKleisli a x; return ((), y)}
                             u () r = modify (adjustOrAdd (+ r) r name)
                            in StatefulKleisliOptic v u,
  evaluate = \(a {- :: Kleisli MCIO x y -} ::- Nil) 
              (StatefulKleisliContext h {- MCIO (z, x) -} 
                                      k {- :: z -> y -> StateT Vector MCIO Payoff -}) ->
    let runInState {- :: x -> y -> MCIO Payoff -} x y
          = do { (z, x') <- h;
                 condition (x == x');
                 (r, v) <- runStateT (k z y) HM.empty;
                 return $ r + HM.findWithDefault 0.0 name v
               }
     in do { xs <- mcSupport numParticles (fmap snd h);
             sequence [ do { strategyExpectation <- mcExpectation numParticles (runKleisli a x >>= runInState x);
                             moveExpectations <- sequence [ do { e <- mcExpectation numParticles (runInState x y);
                                                                 return (y, e)
                                                               }
                                                          | y <- options x ];
                             let {(y, ky) = maximumBy (comparing snd) moveExpectations};
                             return $ DiagnosticInfoMC {
                                equilibrium = strategyExpectation >= ky - epsilon,
                                player = name,
                                optimalMove = y,
                                strategy = runKleisli a x,
                                optimalPayoff = ky,
                                context = runInState x,
                                payoff = strategyExpectation,
                                state = x
                             }
                           }
                      | x <- xs]
        } ::- Nil
}

runInState :: Eq x => StatefulKleisliContext MCIO x () y Double -> String -> x -> y -> MCIO Double
runInState (StatefulKleisliContext h k) {- :: x -> y -> MCIO Payoff -} name x y
          = do { (z, x') <- h;
                 condition (x == x');
                 (r, v) <- runStateT (k z y) HM.empty;
                 return $ r + HM.findWithDefault 0.0 name v
               }

fromLens :: (Monad m) => (x -> y) -> (x -> r -> s) -> StatefulKleisliOpenGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> StatefulKleisliOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}

fromFunctions :: (Monad m) => (x -> y) -> (r -> s) -> StatefulKleisliOpenGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

nature :: (Monad m) => m x -> StatefulKleisliOpenGame m '[] '[] () () x ()
nature a = OpenGame {
  play = \Nil -> StatefulKleisliOptic (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  evaluate = \Nil _ -> Nil}

