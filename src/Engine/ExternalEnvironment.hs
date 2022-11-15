{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Engine.ExternalEnvironment
  ( ExternalEnvironmentGame
  , fromFunctions
  , fromLens
  , interactWithEnv
  , liftStochastic
  , nature )
  
  where

import           Engine.Engine hiding (fromLens, fromFunctions, nature)

---------------------------------------------------------------------
-- This module implements a plain pure input version of an open game.
-- The idea is that the input comes from the outside world.
-- The open games just ties together the information/action flow.
-- Use case is the connection to Ensemble as a game structure for
-- experiments.
---------------------------------------------------------------------

-------------
-- Data types
type AgentEE = String

type ExternalEnvironmentGame a b x s y r = OpenGame (MonadOpticLearning IO) (MonadContextLearning IO) a b x s y r

---------------------
-- Main functionality
-- Takes external input and outputs a move? Does evaluate the game as in the Bayesian case.
interactWithEnv ::
  (Show a, Ord a) =>
   ExternalEnvironmentGame  '[a] '[] x Double a Double
interactWithEnv = OpenGame {
  play =  \(strat ::- Nil) -> let v _ = do
                                   return ((),strat)
                                  u () r = pure r
                                   in MonadOpticLearning v u ,
  evaluate = undefined}

-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> ExternalEnvironmentGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOpticLearning (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}

fromFunctions :: (x -> y) -> (r -> s) -> ExternalEnvironmentGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

-- Nature draws from a random distribution
nature :: IO x -> ExternalEnvironmentGame '[] '[] () () x ()
nature a = OpenGame {
  play = \Nil -> MonadOpticLearning (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  evaluate = \Nil _ -> Nil}

-- Lift a stochastic computation into an open game
liftStochastic :: (x -> IO y) -> ExternalEnvironmentGame '[] '[] x () y ()
liftStochastic f = OpenGame {
  play = \Nil -> MonadOpticLearning (\x -> do {y <- f x; return ((), y)}) (\() () -> return ()),
  evaluate = \_ _ -> Nil}

---------------------------------------------------------------------
-- Utility functions to extract the next state and payoff from a
-- monad optic.
---------------------------------------------------------------------

extractNextState :: MonadOpticLearning m s t a b -> s -> m a
extractNextState (MonadOpticLearning v _) x = do
  (_, nextState) <- v x
  pure nextState

extractPayoffAndNextState :: MonadOpticLearning m s t a b -> s -> b -> m (t, a)
extractPayoffAndNextState (MonadOpticLearning v u) x r = do
  (z, nextState) <- v x
  payoff <- u z r
  pure (payoff, nextState)

