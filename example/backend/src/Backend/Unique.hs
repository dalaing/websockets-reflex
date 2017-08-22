{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Backend.Unique (
    uniqueGuest
  ) where

import Reflex.WebSocket.Server

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex

import Commands
import Util

import Backend.Common

acceptUnique ::
  (GuestConstraintSingle t m, Ord k) =>
  k ->
  WsData () ->
  EventWriterT t (Set k) m ()
acceptUnique k w = mdo
  WebSocket eRead _ _ eClose <- accept w (WebSocketConfig eWrite never)

  dTotal <- foldRequest 0 (decodeRequest eRead)

  let eWrite = encodeResponse . fmap TotalRes . updated $ dTotal

  tellEvent $ Set.singleton k <$ eClose

  pure ()

uniqueGuest ::
  forall t m.
  GuestConstraintGroup t m =>
  Event t (WsData ()) ->
  m ()
uniqueGuest eInsert = mdo

  dId :: Dynamic t Int <- count eInsert

  dModel <- foldDyn ($) Map.empty .
            mergeWith (.) $ [
              Map.insert <$> current dId <@> eInsert
            , flip (foldr Map.delete) <$> eRemoves
            ]

  (_, eRemoves) <-
    runEventWriterT .
    listWithKey dModel $ \k dv -> do
      v <- sample . current $ dv
      acceptUnique k v

  return ()
