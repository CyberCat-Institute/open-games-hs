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


module OpenGames.Engine.Diagnostics
  ( DiagnosticInfoBayesian(..)
  , generateOutput
  , generateOutputString
  , generateIsEq
  , generateIsEqMaybe
  , generateIsEqString
  , generateEquilibrium
  , generatePayoff
  , nextState
  , extractContinuation
  ) where

import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL

import qualified Control.Monad.State  as ST

--------------------------------------------------------
-- Diagnosticinformation and processesing of information
-- for standard game-theoretic analysis

-- Defining the necessary types for outputting information of a BayesianGame
data DiagnosticInfoBayesian x y = DiagnosticInfoBayesian
  { equilibrium     :: Bool
  , player          :: String
  , optimalMove     :: y
  , strategy        :: Stochastic y
  , optimalPayoff   :: Double
  , context         :: (y -> Double)
  , payoff          :: Double
  , state           :: x
  , unobservedState :: String}


-- prepare string information for Bayesian game
showDiagnosticInfo :: (Show y, Ord y, Show x) => DiagnosticInfoBayesian x y -> String
showDiagnosticInfo info =  
     "\n"    ++ "Player: " ++ player info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMove info)
     ++ "\n" ++ "Current Strategy: " ++ (show $ strategy info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoff info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ payoff info)
     ++ "\n" ++ "Observable State: " ++ (show $ state info)
     ++ "\n" ++ "Unobservable State: " ++ (show $ unobservedState info)



-- output string information for a subgame expressions containing information from several players - bayesian 
showDiagnosticInfoL :: (Show y, Ord y, Show x) => [DiagnosticInfoBayesian x y] -> String
showDiagnosticInfoL [] = "\n --No more information--"
showDiagnosticInfoL (x:xs)  = showDiagnosticInfo x ++ "\n --other game-- " ++ showDiagnosticInfoL xs 

-- checks equilibrium and if not outputs relevant deviations
checkEqL :: (Show y, Ord y, Show x) => [DiagnosticInfoBayesian x y] -> String
checkEqL ls = let xs = fmap equilibrium ls
                  ys = filter (\x -> equilibrium x == False) ls
                  isEq = and xs
                  in if isEq == True then "\n Strategies are in equilibrium"
                                      else "\n Strategies are NOT in equilibrium. Consider the following profitable deviations: \n"  ++ showDiagnosticInfoL ys

-- checks equilibrium for the branching case
-- checks equilibrium and if not outputs relevant deviations
checkEqMaybeL :: (Show y, Ord y, Show x) => Maybe [DiagnosticInfoBayesian x y] -> String
checkEqMaybeL ls =
  case ls of
    Just ls' -> checkEqL ls'
    Nothing  -> "\n NOTHING CASE"
    

-- map diagnostics to equilibrium
toEquilibrium :: DiagnosticInfoBayesian x y -> Bool
toEquilibrium = equilibrium

equilibriumMap :: [DiagnosticInfoBayesian x y] -> Bool
equilibriumMap = and . fmap toEquilibrium

-- map diagnostics to payoff
toPayoff :: DiagnosticInfoBayesian x y -> Double
toPayoff = payoff

payoffMap :: [DiagnosticInfoBayesian x y] -> [Double]
payoffMap = fmap toPayoff



----------------------------------------------------------
-- providing the relevant functionality at the type level
-- for show output

data ShowDiagnosticOutput = ShowDiagnosticOutput

instance (Show y, Ord y, Show x) => Apply ShowDiagnosticOutput [DiagnosticInfoBayesian x y] String where
  apply _ x = showDiagnosticInfoL x


data PrintIsEq = PrintIsEq

instance (Show y, Ord y, Show x) => Apply PrintIsEq [DiagnosticInfoBayesian x y] String where
  apply _ x = checkEqL x

-- for the branching operator
data PrintIsEqMaybe = PrintIsEqMaybe

instance (Show y, Ord y, Show x) => Apply PrintIsEq (Maybe [DiagnosticInfoBayesian x y]) String where
  apply _ x = checkEqMaybeL x

data PrintOutput = PrintOutput

instance (Show y, Ord y, Show x) => Apply PrintOutput [DiagnosticInfoBayesian x y] String where
  apply _ x = showDiagnosticInfoL x

data Concat = Concat

instance Apply Concat String (String -> String) where
  apply _ x = \y -> x ++ "\n NEWGAME: \n" ++ y

-- for apply output of equilibrium function
data Equilibrium = Equilibrium 

instance Apply Equilibrium [DiagnosticInfoBayesian x y] Bool where
  apply _ x = equilibriumMap x

data And = And

instance Apply And Bool (Bool -> Bool) where
  apply _ x = \y -> y && x

-- for apply output of equilibrium function
data Payoff = Payoff

instance Apply Payoff [DiagnosticInfoBayesian x y] [Double] where
  apply _ x = payoffMap x

data Identity = Identity

instance Apply Identity [Double] [Double] where
  apply _ x = fmap id x


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
      MapL Payoff xs (ConstMap [Double] xs)) =>
     List xs -> [[Double]]
generatePayoff hlist = mapToDoubles $ mapListToDouble hlist
 where mapListToDouble :: forall xs.
            (MapL Payoff xs (ConstMap [Double] xs))
            => List xs -> List (ConstMap [Double] xs)
       mapListToDouble hlist =  mapL @_ @_ @(ConstMap [Double] xs) Payoff hlist
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

extractContinuation
  :: StochasticStatefulOptic s t a ()
     -> s
     -> ST.StateT Vector Stochastic t
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()




