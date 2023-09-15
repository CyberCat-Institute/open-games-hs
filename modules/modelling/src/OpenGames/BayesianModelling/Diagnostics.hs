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


module OpenGames.BayesianModelling.Diagnostics
  ( generateOutput
  , generateOutputString
  , generateIsEq
  , generateIsEqMaybe
  , generateIsEqString
  , generateEquilibrium
  , generatePayoff
  , nextState
  , nextContinuation
  ) where

import OpenGames.Engine.Optics.StochasticStateful
import OpenGames.Engine.TLL
import OpenGames.BayesianModelling.Diagnostics.Printer
import OpenGames.BayesianModelling.Diagnostics.TLLTypes

import qualified Control.Monad.State  as ST

---------------------
-- main functionality

-- print all information for all players
generateOutput :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateOutput hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"

-- generate string of output information for all information for all players
generateOutputString :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> String
generateOutputString hlist =
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"



-- print output equilibrium relevant information
generateIsEq :: forall xs.
               ( MapL   PrintIsEq xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateIsEq hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintIsEq hlist) ++ "----Analytics end----\n"

-- print output equilibrium relevant information
generateIsEqMaybe :: forall xs.
               ( MapL   PrintIsEqMaybe xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateIsEqMaybe hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintIsEqMaybe hlist) ++ "----Analytics end----\n"
 
-- print string output equilibrium relevant information
generateIsEqString :: forall xs.
               ( MapL   PrintIsEq xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> String
generateIsEqString hlist =
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintIsEq hlist) ++ "----Analytics end----\n"



-- give equilibrium value for further use
generateEquilibrium :: forall xs.
               ( MapL   Equilibrium xs     (ConstMap Bool xs)
               , FoldrL And Bool (ConstMap Bool xs)
               ) => List xs -> Bool
generateEquilibrium hlist = foldrL And True $ mapL @_ @_ @(ConstMap Bool xs) Equilibrium hlist

-- give achieved payoffs with current strategy
generatePayoff
  :: (MapListPayoff Identity (ConstMap [Double] xs),
      MapL Payoffs xs (ConstMap [Double] xs)) =>
     List xs -> [[Double]]
generatePayoff hlist = mapToDoubles $ mapListToDouble hlist
 where mapListToDouble :: forall xs.
            (MapL Payoffs xs (ConstMap [Double] xs))
            => List xs -> List (ConstMap [Double] xs)
       mapListToDouble hlist =  mapL @_ @_ @(ConstMap [Double] xs) Payoffs hlist
       mapToDoubles :: MapListPayoff Identity xs => List xs -> [[Double]]
       mapToDoubles hlist = mapListPayoff Identity hlist

---------------------------------------
-- Helper functionality for play output

-- Transform the optic into the next state given some input
nextState ::
  StochasticStatefulOptic s t a b ->
  s ->
  Stochastic a
nextState (StochasticStatefulOptic v _) x = do
  (z, a) <- v x
  pure a

nextContinuation
  :: StochasticStatefulOptic s t a ()
     -> s
     -> ST.StateT Vector Stochastic t
nextContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()




