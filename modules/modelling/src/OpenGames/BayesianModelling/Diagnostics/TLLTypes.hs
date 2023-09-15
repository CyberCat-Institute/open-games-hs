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


module OpenGames.BayesianModelling.Diagnostics.TLLTypes
  ( ShowDiagnosticOutput(..)
  , PrintIsEq(..)
  , PrintIsEqMaybe(..)
  , PrintOutput(..)
  , Concat(..)
  , Equilibrium(..)
  , And(..)
  , Payoffs(..)
  , Identity(..)
  ) where


import OpenGames.Engine.OpenGames.BayesianGames 
import OpenGames.Engine.Optics.StochasticStateful 
import OpenGames.Engine.TLL
import OpenGames.BayesianModelling.Diagnostics.Printer

{-
Types for the Diagnostics of Bayesian games, including TLL constructions
-}

----------------------------------------------------------
-- providing the relevant functionality at the type level

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
data Payoffs = Payoffs

instance Apply Payoffs [DiagnosticInfoBayesian x y] [Double] where
  apply _ x = payoffMap x

data Identity = Identity

instance Apply Identity [Double] [Double] where
  apply _ x = fmap id x


