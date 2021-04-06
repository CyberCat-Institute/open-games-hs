{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.Auctions.SequentialAuction where

import Control.Arrow (Kleisli(..))
import Data.List
import Language.Haskell.TH
import System.Console.Haskeline
import Text.Read

import Engine.AtomicGames
import Engine.BayesianGames
import Engine.OpenGames
import Engine.OpticClass
import Engine.TLL
import Preprocessor.THSyntax
import Preprocessor.AbstractSyntax
import Preprocessor.Compile




-- TODO Generalize to population of players

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double 

values = [10,30,60]

reservePrice = 1

----------------------
-- 1 Preliminary stuff

-- Order bids from large to small
allocation :: [(String, Values)] -> [(String, Values)]
allocation  = sortBy (flip (\(_,v1) (_,v2) -> compare v1 v2 ))

-- Determine k-max bid
kMaxBid :: Int -> [(String, Values)] ->  Values
kMaxBid k ls = snd $  allocation ls !! (k-1)

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
setPaymentKMax :: Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)]
setPaymentKMax _        _    _         _             _               []                     = []
setPaymentKMax resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then (name,kmax) : setPaymentKMax resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : setPaymentKMax resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : setPaymentKMax resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls


-- Mark the auctionWinners 
auctionWinner :: Values -> [(String, Values)] -> [(String, Values,Bool)]
auctionWinner kmax []= []
auctionWinner kmax ls@((name,b):bs)=
  if b < kmax then (name,b,False) : auctionWinner kmax bs
              else (name,b,True)  : auctionWinner kmax bs

-- Determine payments for winners; for lottery winners, and for those who do not get a good set it to 0
setPayment :: Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)]
setPayment _        _    _         _             _               []                     = []
setPayment resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then
           if counterWinner < noLottery then (name,resPrice) : setPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
                                        else (name,kmax) : setPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : setPayment resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : setPayment resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls

-- Select the payment for a player given the list of payments
selectPayoffs :: String -> [(String,Values)] -> Values
selectPayoffs name [] = 0
selectPayoffs name ((n,p):ls) = if name == n then p else selectPayoffs name ls

-- Determines the payoff for each player
setPayoff :: (String,Values) -> [(String, Values)] -> Values
setPayoff (name,value) payments =
  if pay == 0 then 0 else value - pay
  where
    pay =  selectPayoffs name payments



-- Determine the payments given k slots being allocated through auction; and _noLotteries_ slots through lottery
auctionPayment :: (Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)])
                 -- ^ Payment function 
                 -> Int -> Int -> [(String, Values)]
                 -- ^ Parameters
                 -> [(String, Values)]
auctionPayment paymentFunction k noLotteries ls  = paymentFunction reservePrice kmax noLotteries 0 0 (auctionWinner kmax ls)
   where kmax = kMaxBid k ls


-- Random shuffle of bids
shuffleBids :: [(String, Values)] -> Stochastic [(String, Values)]
shuffleBids ls = uniformDist $ permutations ls

---------------------
-- 1 The actual games

-- Draws a value and creates a pair of _value_ _name_
natureDrawsTypeStage name = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value ;
    returns   :  ;
    :-----:

    outputs   :  (name,value) ;
    returns   :    ;
  |]

-- Transforms the payments into a random reshuffling
transformPayments k noLotteries paymentFunction = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : liftStochasticForward shuffleBids ;
   outputs   : shuffledBids ;
   returns   :      ;

   inputs    : shuffledBids ;
   feedback  :      ;
   operation : forwardFunction (auctionPayment paymentFunction k noLotteries) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]



-- Instantiates a simplified version with three players
bidding3 k noLotteries paymentFunction = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Alice" ;
   outputs   :  aliceValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Bob" ;
   outputs   :  bobValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Carol" ;
   outputs   :  carolValue ;
   returns   :      ;

   inputs    :  aliceValue    ;
   feedback  :      ;
   operation :  dependentDecision "Alice" (const [0,20..60]) ;
   outputs   :  aliceDec ;
   returns   :  setPayoff aliceValue payments  ;

   inputs    :  bobValue    ;
   feedback  :      ;
   operation :  dependentDecision "Bob" (const [0,20..60]) ;
   outputs   :  bobDec ;
   returns   :  setPayoff bobValue payments  ;

   inputs    :  carolValue    ;
   feedback  :      ;
   operation :  dependentDecision "Carol" (const [0,20..60]) ;
   outputs   :  carolDec ;
   returns   :  setPayoff carolValue payments  ;

   inputs    :  [("Alice",aliceDec),("Bob",bobDec),("Carol",carolDec)]  ;
   feedback  :      ;
   operation :   transformPayments k noLotteries paymentFunction ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]
 
-- B Analysis
-------------

---------------
-- 0 Strategies

-- Truthful bidding
stratBidderTruth   = Kleisli $ (\(_,x) -> certainly x)

-- Bidding strategy with threshold 50 and value 10
stratBidderThreshold = Kleisli $ (\(_,x) -> if x >= 50 then certainly 20 else certainly 10)

-- Bidding with different threshold
stratBidderThreshold' = Kleisli $ (\(_,x) -> case () of
                                              _ | x < 30 -> certainly 0
                                                | x == 30 -> certainly 10 
                                                | otherwise ->  certainly 20)



-- Complete strategy for truthful bidding for 5 players
truthfulStrat5 ::
  List
   '[ Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat5 =
     stratBidderTruth
  ::- stratBidderTruth
  ::- stratBidderTruth
  ::- stratBidderTruth
  ::- stratBidderTruth
  ::- Nil

-- Complete strategy for threshold bidding 5 players
thresholdStrat5 ::
  List
   '[ Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
thresholdStrat5 =
     stratBidderThreshold
  ::- stratBidderThreshold
  ::- stratBidderThreshold
  ::- stratBidderThreshold
  ::- stratBidderThreshold
  ::- Nil

-- Complete strategy for truthful bidding for 3 players
truthfulStrat3 ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat3 =
  stratBidderTruth
  ::- stratBidderTruth
  ::- stratBidderTruth
  ::- Nil

-- Complete strategy for threshold bidding 3 players
thresholdStrat3 ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
thresholdStrat3 =
  stratBidderThreshold
  ::- stratBidderThreshold
  ::- stratBidderThreshold
  ::- Nil
  
-- Complete strategy for threshold' bidding 3 players
thresholdStrat3' ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
thresholdStrat3' =
  stratBidderThreshold'
  ::- stratBidderThreshold'
  ::- stratBidderThreshold'
  ::- Nil

---------------
-- 1 Equilibria
-- follows payment rule in  Proposal (https://notes.ethereum.org/@barnabe/S1eUmr72I)

-- 1.0 Eq. game with 5 players
-- Interacting with the outside world
equilibriumGame5 k noLotteries strat = evaluate (bidding5 k noLotteries setPayment) strat void


-- 1.1 Eq. game with 3 players
equilibriumGame3 k noLotteries strat = evaluate (bidding3 k noLotteries setPayment) strat void


---------------
-- 2 Equilibria
-- follows k-price auction

-- 2.0 Eq. game with 5 players
-- Interacting with the outside world
equilibriumGame5KMax k noLotteries strat = evaluate (bidding5 k noLotteries setPaymentKMax) strat void


-- 2.1 Eq. game with 3 players
equilibriumGame3KMax k noLotteries strat = evaluate (bidding3 k noLotteries setPaymentKMax) strat void



------------------------
-- 2 Interactive session


-- 3 players with 1 auction slot and 1 lottery slot - truthful bidding - not an eq
-- (equilibriumGame3 1 1 truthfulStrat3)

-- 3 players with 1 auction slot and 1 lottery slot - threshold bidding - not an eq
-- (equilibriumGame3 1 1 thresholdStrat3)

-- NOTE how to come from the non equilibrium to the equilibrium
-- Let us illustrate this with a different payment rule

-- 3 players with 1 auction slot and 1 lottery slot AND 2nd price rule - strategies before not an equilibrium
-- mapM print $ equilibriumGame3KMax 1 1 thresholdStrat3

-- Truthful bidding is also not an equilibrium
-- mapM print $ equilibriumGame3KMax 1 1 truthfulStrat3

-- But the alternative threshold strategy is
-- mapM print $ equilibriumGame3KMax 1 1 thresholdStrat3'


