{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Backend.SharedDB (
    sharedGuestDB
  ) where

import Control.Monad (void)
import Data.Maybe (mapMaybe)

import Control.Lens (Prism', prism, preview, review)

import Data.Monoid (Sum(..), Any(..), Last(..))

import qualified Data.Map as Map

import qualified Data.Set as Set

import Reflex hiding (select)

import Reflex.Selda
import Database.Selda hiding (Set, count)
import Database.Selda.SQLite

import Reflex.WebSocket.Server

import Commands
import Util

import Backend.Common
import Backend.Shared

eventTable :: Table (RowID :*: Maybe Int :*: Maybe Bool)
(eventTable, etID :*: etAdd :*: etCount)
  = tableWithSelectors "events"
  $   autoPrimary "id"
  :*: optional "add"
  :*: optional "clear"

snapshotTable :: Table (RowID :*: Int)
(snapshotTable, stID :*: stTotal)
  = tableWithSelectors "snapshot"
  $    primary "id"
  :*:  required "total"

{-
loadSnapshot ::
  MonadSelda m =>
  m (Maybe (RowID :*: Int))
loadSnapshot = do
  tryCreateTable snapshotTable

  -- try to read the (old-last-event-id, old-snapshot) from the snapshot table
  res <- query $ do
    (i :*: s) <- select snapshotTable
    order i descending
    pure (i :*: s)
  -- - want the one with the largest event id

  -- read the events after old-last-event-id and apply them to old-snapshot
  -- - also want to find the largest event id amongst these events

--  operation to load (row, int) from eventTable and snapshotTable
  pure _

saveSnapshot ::
  MonadSelda m =>
  (RowID :*: Int) ->
  m ()
saveSnapshot (r :*: i) = do
  tryCreateTable snapshotTable

  -- write (last-event-id, snapshot) to the snapshot table
  -- delete the old snapshot value
  -- delete the events up to and including last-event-id

-- operation to save (row, int) to snapshotTable and then clean up eventTable and snapshotTable
  pure _

vacuumEventLog ::
  MonadSelda m =>
  m (Maybe (RowID :*: Int))
vacuumEventLog = do
  x <- loadSnapshot
  maybe (pure ()) saveSnapshot x
  pure x
-}

_Request :: Prism' (Maybe Int :*: Maybe Bool) CommandRequest
_Request =
  let
    toState (AddReq i) = Just i :*: Nothing
    toState ClearReq = Nothing :*: Just True
    fromState (Just i :*: _) = Right $ AddReq i
    fromState (_ :*: Just _) = Right $ ClearReq
    fromState t = Left t
  in
    prism toState fromState

gatherRequests :: [RowID :*: Maybe Int :*: Maybe Bool] -> Int
gatherRequests =
  let
    t (_ :*: xs) = xs
  in
    foldr applyRequest 0 .
    reverse .
    mapMaybe (preview _Request . t)

getEvents :: MonadSelda m => m [RowID :*: Maybe Int :*: Maybe Bool]
getEvents =
  query (select eventTable)

sharedGuestDB ::
  forall t m.
  (GuestConstraintGroup t m, SeldaEvent t m) =>
  Event t (WsData ()) ->
  m (Dynamic t Int)
sharedGuestDB eInsert = mdo
  ePostBuild <- getPostBuild
  (eError1, eCreated) <- seldaEvent $ tryCreateTable eventTable <$ ePostBuild
  (eError2, eLoad) <- seldaEvent $ getEvents <$ eCreated

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
    eInitial =
      fmap gatherRequests eLoad
    eRequest =
      fmapMaybe (getLast . swRequest) eSharedWriter
    eRemoves =
      fmap swClose eSharedWriter
    dSize =
      fmap Map.size dModel

  (eError3, eRequestInserted) <-
    insertEvent eventTable (\x -> def :*: review _Request x) eRequest

  dTotal <- foldRequestE eInitial eRequestInserted

  performEvent_ $ (liftIO . print) <$> leftmost [eError1, eError2, eError3]

  return dSize
