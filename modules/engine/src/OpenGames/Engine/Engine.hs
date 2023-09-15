{-# LANGUAGE ExplicitNamespaces #-}

module OpenGames.Engine.Engine
  ( nature
  , natureEndInput
  , StochasticStatefulBayesianOpenGame(..)
  , Agent(..)
  , Payoff(..)
  , dependentDecision
  , dependentRoleDecision
  , dependentEpsilonDecision
  , fromFunctions
  , fromLens
  , uniformDist
  , distFromList
  , pureAction
  , playDeterministically
  , discount
  , addPayoffs
  , addRolePayoffs
  , DiagnosticInfoBayesian(..)
  , generateOutput
  , generateOutputString
  , generateIsEq
  , generateIsEqMaybe
  , generateIsEqString
  , generateEquilibrium
  , generatePayoff
  , nextState
  , nextContinuation 
  , OpenGame(..)
  , lift
  , reindex
  , (>>>)
  , (&&&)
  , (+++)
  , Stochastic(..)
  , Vector(..)
  , StochasticStatefulOptic(..)
  , StochasticStatefulContext(..)
  , Optic(..)
  , Precontext(..)
  , Context(..)
  , ContextAdd(..)
  , identity
  , List(..)
  , Apply(..)
  , Unappend(..)
  , MapL(..)
  , FoldrL(..)
  , ConstMap(..)
  , SequenceList(..)
  , Natural(..)
  , IndexList(..)
  , type (+:+)
  , (+:+)
  , Kleisli(..)
  ) where

-- | File organizes the imports of the engine to streamline the import of relevant functionality
import OpenGames.Engine.BayesianGames hiding (liftStochastic)
import OpenGames.Engine.OpenGames
import OpenGames.Engine.Optics
import OpenGames.Engine.Optics.StochasticStateful
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.TLL

import Control.Arrow (Kleisli(..))
