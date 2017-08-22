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
module Backend.Shared (
    SharedWriter(..)
  , acceptShared
  , sharedGuest
  ) where

import Control.Monad (void)

import Data.Semigroup hiding (Last(..))
import Data.Monoid (Last(..))

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex

import Reflex.WebSocket.Server

import Commands
import Util

import Backend.Common

data SharedWriter k =
  SharedWriter {
    swRequest :: Last CommandRequest
  , swClose   :: Set k
  }

mkRequest ::
  Ord k =>
  CommandRequest ->
  SharedWriter k
mkRequest req =
  SharedWriter (pure req) mempty

mkClose ::
  Ord k =>
  k ->
  SharedWriter k
mkClose k =
  SharedWriter mempty (Set.singleton k)

instance Ord k => Semigroup (SharedWriter k) where
  (SharedWriter a1 b1) <> (SharedWriter a2 b2) =
    SharedWriter
      (a1 <> a2)
      (b1 <> b2)

instance Ord k => Monoid (SharedWriter k) where
  mempty = SharedWriter mempty mempty
  mappend = (<>)

acceptShared ::
  ( GuestConstraintSingle t m
  , Ord k
  )=>
  k ->
  Dynamic t Int ->
  WsData a ->
  EventWriterT t (SharedWriter k) m ()
acceptShared k dTotal w = mdo
  WebSocket eRead eOpen _ eClose <- accept w (WebSocketConfig eWrite never)

  let
    eWrite = encodeResponse .
             fmap TotalRes .
             leftmost $ [
                 updated dTotal
               , current dTotal <@ eOpen
               ]

  tellEvent $ mkRequest <$> decodeRequest eRead
  tellEvent $ mkClose k <$  eClose

  pure ()

sharedGuest ::
  forall t m a.
  GuestConstraintGroup t m =>
  Event t (WsData a) ->
  m (Dynamic t Int)
sharedGuest eInsert = mdo

  dId :: Dynamic t Int <- count eInsert

  dModel <- foldDyn ($) Map.empty .
            mergeWith (.) $ [
              Map.insert <$> current dId <@> eInsert
            , flip (foldr Map.delete) <$> eRemoves
            ]

  (_, eSharedWriter) <-
    runEventWriterT .
    listWithKey dModel $ \k dv -> do
      v <- sample . current $ dv
      acceptShared k dTotal v

  let
    eRequest =
      fmapMaybe (getLast . swRequest) eSharedWriter
    eRemoves =
      fmap swClose eSharedWriter
    dSize =
      fmap Map.size dModel

  dTotal <- foldRequest 0 eRequest

  return dSize
