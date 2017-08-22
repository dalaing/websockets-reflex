{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Backend.Indexed (
    indexedGuest
  ) where

import qualified Data.Map as Map

import Reflex

import Reflex.WebSocket.Server

import Util

import Backend.Common
import Backend.Shared

indexedGuest ::
  GuestConstraintGroup t m =>
  Event t (WsData Int) ->
  m ()
indexedGuest eInsert = mdo

  let
    ePair =
      (\w -> (wsPayload w, sharedGuest eInsert)) <$> eInsert

  dModel <- foldDyn ($) Map.empty .
            mergeWith (.) $ [
              uncurry Map.insert <$> ePair
            , flip (foldr Map.delete) <$> eRemoves
            ]

  dMap <- listWithKey dModel $ \_ dv -> do
            v <- sample . current $ dv
            v

  let eRemoves =
        fmap Map.keys .
        updated .
        fmap (Map.filter (== 0)) .
        joinDynThroughMap $
        dMap

  pure ()
