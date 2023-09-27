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


module OpenGames.MonteCarloModelling.Diagnostics
  (   ) where

import OpenGames.Engine.Optics.StatefulKleisli
import OpenGames.Engine.TLL

import qualified Control.Monad.State  as ST

---------------------
-- TODO

