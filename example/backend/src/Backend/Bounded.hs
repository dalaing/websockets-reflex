{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Backend.Bounded (
    boundedGuest
  ) where

import Control.Monad (void)

import qualified Data.Map as Map

import Reflex

import Reflex.WebSocket.Server

import Util

import Backend.Common
import Backend.Shared

boundedGuest ::
  forall t m.
  GuestConstraintGroup t m =>
  Int ->
  Event t (WsData ()) ->
  m (Dynamic t Int)
boundedGuest bound eInsert = mdo

  let
    bHasSpace = (< bound) <$> current dSize
    eInsertHasSpace = gate bHasSpace eInsert
    eInsertHasNoSpace = gate (not <$> bHasSpace) eInsert

  reject "too many connections" eInsertHasNoSpace

  dSize <- sharedGuest eInsertHasSpace

  pure dSize
