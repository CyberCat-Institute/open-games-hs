{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Modelling.AtomicGames
 ( decision
 , decisionNoObs
 , forwardFunction
 , backwardFunction
 , natureDraw
 , liftStochasticForward
 ) where

import OpenGames.Engine.BayesianGames
import OpenGames.Engine.OpenGames
import OpenGames.Engine.Optics
import OpenGames.Preprocessor

---------------------------------------
-- 0. A single action making a generic -- parameterized -- decision
decision actionSpace payoffFunction playerName = [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x ;
    feedback  :   ;
    operation : dependentDecision playerName (\y -> actionSpace) ;
    outputs   : y ;
    returns   : payoffFunction y x r ;
    :-----:

    outputs  : y ;
    returns  : r ;

|]

-- 1. A single action making a decision without prior observations
decisionNoObs actionSpace payoffFunction playerName = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : dependentDecision playerName (\y -> actionSpace) ;
    outputs   : y ;
    returns   : payoffFunction y r ;
    :-----:

    outputs  : y ;
    returns  : r ;

|]


-- 2. "Forward" (covariant) function: from past to future
forwardFunction function = [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x  ;
    feedback  :   ;
    operation : fromFunctions function id ;
    outputs   : y ;
    returns   :   ;
    :-----:

    outputs  : y ;
    returns  :   ;

|]

 -- 3. "Backward" (contravariant) function: from future to past
backwardFunction function = [opengame|

    inputs    :   ;
    feedback  : s ;

    :-----:
    inputs    :   ;
    feedback  : s;
    operation : fromFunctions id function ;
    outputs   :  ;
    returns   : r ;
    :-----:

    outputs  :    ;
    returns  :  r ;

|]

-- 4. Drawing from a probability distribution
natureDraw distribution =  [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature distribution ;
    outputs   : draw ;
    returns   :  ;
    :-----:

    outputs   :  draw  ;
    returns   :    ;

|]

-- 5. lift a stochasticProcess forward
liftStochasticForward process =  [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x ;
    feedback  :   ;
    operation : liftStochastic process;
    outputs   : draw ;
    returns   :   ;
    :-----:

    outputs   : draw  ;
    returns   :    ;

|]


