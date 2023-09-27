module OpenGames.Engine.Types
 ( Agent
 , Payoff
 , Vector
 ) where

import qualified Data.HashMap as HM

type Vector = HM.Map String Double

type Agent = String

type Payoff = Double

