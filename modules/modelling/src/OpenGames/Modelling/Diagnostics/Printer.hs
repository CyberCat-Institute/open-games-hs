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


module OpenGames.Modelling.Diagnostics.Printer
  ( showDiagnosticInfo
  , showDiagnosticInfoL
  , checkEqL
  , checkEqMaybeL
  , toEquilibrium
  , equilibriumMap
  , toPayoff
  , payoffMap
  ) where

import OpenGames.Modelling.Diagnostics.Types

{-
Provides output printing facilities as well as 
-}

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

-- TODO move parts to relevant place
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

