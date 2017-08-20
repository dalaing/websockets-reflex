{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend (
    go
  ) where

import Control.Monad (void)
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Monoid (Any(..), Sum(..))

import Control.Monad.Fix (MonadFix)
import Control.Exception (finally)
import Control.Concurrent (forkIO, killThread)

import Control.Monad.STM (STM, atomically)

import Control.Monad.Trans (MonadIO(..))

import Data.Binary (encode, decode)

import Control.Lens hiding (indexed)

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex hiding (select)

import Reflex.WebSocket.Server (WsManager(..))
import Reflex.WebSocket.Server.Snap
import Reflex.Basic.Host

import Reflex.Selda
import Database.Selda hiding (Set, count)
import Database.Selda.SQLite

import Commands

import Util

data Counter t =
  Counter {
    cAdd :: Event t Int
  , cClear :: Event t ()
  }

mkCounter :: Reflex t
          => Event t B.ByteString
          -> Counter t
mkCounter eReadBs =
  let
    eRead = fmap (decode . LB.fromStrict) eReadBs

    isAdd (AddReq x) = Just x
    isAdd _ = Nothing
    eAdd = fmapMaybe isAdd eRead

    isClear ClearReq = Just ()
    isClear _ = Nothing
    eClear = fmapMaybe isClear eRead
  in
    Counter eAdd eClear

gatherDynMap :: ( Ord k
                , Reflex t
                )
             => (v -> Event t a)
             -> (Map k a -> b)
             -> Dynamic t (Map k v)
             -> Event t b
gatherDynMap f g =
  -- switch .
  -- fmap (fmap g . mergeMap . fmap f) .
  -- current
  fmap g .
  switch .
  current .
  fmap (mergeMap . fmap f)

type GuestConstraintSingle t m =
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , PostBuild t m
  , MonadIO (Performable m)
  , MonadIO m
  )

type GuestConstraintGroup t m =
  ( GuestConstraintSingle t m
  , MonadAdjust t m
  )

data Unique t =
  Unique {
    uClose :: Event t ()
  }

acceptUnique :: GuestConstraintSingle t m
             => WsData ()
             -> m (Unique t)
acceptUnique w = mdo
  WebSocket eReadBs _ _ eClose <- accept w (WebSocketConfig eWrite never)
  let Counter eAdd eClear = mkCounter eReadBs

  dTotal <- foldDyn ($) (0 :: Int) . mergeWith (.) $ [
              (+) <$> eAdd
            , const 0 <$ eClear
            ]

  let
    eWrite = fmap (pure . LB.toStrict . encode . TotalRes) .
             updated $
             dTotal

  return $ Unique (() <$ eClose)

uniqueGuest :: GuestConstraintGroup t m
            => Event t (WsData ())
            -> m ()
uniqueGuest eInsert = mdo

  dId <- count eInsert

  dModel <- foldDyn ($) Map.empty . 
            mergeWith (.) $ [
              Map.insert <$> current dId <@> eInsert
            , flip (foldr Map.delete) <$> eRemoves
            ]

  dMap <- listWithKey dModel $ \_ dv -> do
            v <- sample . current $ dv
            acceptUnique v

  let
    eRemoves = 
      gatherDynMap uClose Map.keys dMap

  return ()

data Shared t =
  Shared {
    sAdd :: Event t Int
  , sClear :: Event t ()
  , sClose :: Event t ()
  }

acceptShared :: GuestConstraintSingle t m
             => Dynamic t Int
             -> WsData ()
             -> m (Shared t)
acceptShared dTotal w = mdo
  WebSocket eReadBs eOpen _ eClose <- accept w (WebSocketConfig eWrite never)
  let Counter eAdd eClear = mkCounter eReadBs

  let
    eWrite = fmap (pure . LB.toStrict . encode . TotalRes) .
             leftmost $ [
               updated dTotal
             , tag (current dTotal) eOpen
             ]

  return $ Shared eAdd eClear (() <$ eClose)

sharedGuest :: GuestConstraintGroup t m
            => Event t (WsData ())
            -> m (Event t ())
sharedGuest eInsert = mdo

  dId <- count eInsert

  dModel <- foldDyn ($) Map.empty . 
            mergeWith (.) $ [
              Map.insert <$> current dId <@> eInsert
            , flip (foldr Map.delete) <$> eRemoves
            ]

  dMap <- listWithKey dModel $ \_ dv -> do
            v <- sample . current $ dv
            acceptShared dTotal v

  let
    eRemoves = 
      gatherDynMap sClose Map.keys dMap
    eAdd =
      gatherDynMap sAdd (sum . Map.elems) dMap
    eClear =
      gatherDynMap sClear void dMap

  dTotal <- foldDyn ($) 0 .
            mergeWith (.) $ [
              (+) <$> eAdd
            , const 0 <$ eClear
            ]

  let
    eEmpty =
      void .
      ffilter Map.null $
      (current dMap <@ eRemoves)

  return eEmpty

boundedGuest :: GuestConstraintGroup t m
             => Int
             -> Event t (WsData ())
             -> m ()
boundedGuest bound eb = mdo
  bId :: Behavior t Int <- accum (flip ($)) (0 :: Int) (succ <$ eb)

  let
    bHasSpace =
      (\m -> Map.size m < bound) <$> current dMap
    eInsert =
      (\k -> Map.singleton k . Just) <$> bId <@> gate bHasSpace eb
    eDeletes =
      gatherDynMap sClose (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]

  reject "too many connections" (gate (not <$> bHasSpace) eb)
  dMap <- listHoldWithKey Map.empty eMap (const $ acceptShared dTotal)

  let
    eAdd =
      gatherDynMap sAdd (sum . Map.elems) dMap
    eClear =
      gatherDynMap sClear (() <$) dMap

  dTotal <- foldDyn ($) 0 .
            mergeWith (.) $ [
              (+) <$> eAdd
            , const 0 <$ eClear
            ]

  return ()

indexedGuest :: GuestConstraintGroup t m
              => Event t (WsData Int)
              -> m ()
indexedGuest ei = mdo
  let
    newInserts =
      fmap snd . ffilter fst $ (\m k -> (Map.notMember k m, k)) <$> current dMap <@> fmap wsPayload ei
    connectsForKey k =
      fmap (() <$) . ffilter ((== k) . wsPayload) $ ei
    eInsert =
      (\k -> Map.singleton k . Just $ connectsForKey k) <$> newInserts
    eDeletes =
      gatherDynMap id (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]
  dMap <- listHoldWithKey Map.empty eMap (const sharedGuest)

  return ()

data SharedDB t =
  SharedDB {
    sdAdd :: Event t Int
  , sdClear :: Event t ()
  , sdClose :: Event t ()
  }

data SharedDbWriter k =
  SharedDbWriter {
    _sdwAdd :: Sum Int
  , _sdwClear :: Any
  , _sdwClose :: Set k
  }

makeLenses ''SharedDbWriter

mkAdd ::
  Ord k =>
  Int ->
  SharedDbWriter k
mkAdd x =
  mempty &
    sdwAdd %~ mappend (Sum x)

mkClear ::
  Ord k =>
  SharedDbWriter k
mkClear =
  mempty &
    sdwClear %~ mappend (Any True)

mkClose ::
  Ord k =>
  k ->
  SharedDbWriter k
mkClose k =
  mempty &
    sdwClose %~ mappend (Set.singleton k)

instance Ord k => Semigroup (SharedDbWriter k) where
  (SharedDbWriter a1 b1 c1) <> (SharedDbWriter a2 b2 c2) =
    SharedDbWriter
      (a1 <> a2)
      (b1 <> b2)
      (c1 <> c2)

instance Ord k => Monoid (SharedDbWriter k) where
  mempty = SharedDbWriter mempty mempty mempty
  mappend = (<>)

acceptSharedDB :: (Ord k, GuestConstraintSingle t m, SeldaEvent t m)
               => k
               -> Dynamic t Int
               -> WsData ()
               -> EventWriterT t (SharedDbWriter k) m ()
acceptSharedDB k dTotal w = mdo
  WebSocket eReadBs eOpen _ eClose <- accept w (WebSocketConfig eWrite never)
  let Counter eAdd eClear = mkCounter eReadBs

  let
    eWrite = fmap (pure . LB.toStrict . encode . TotalRes) .
             leftmost $ [
               updated dTotal
             , tag (current dTotal) eOpen
             ]

  tellEvent $ mkAdd <$> eAdd
  tellEvent $ mkClear <$ eClear
  tellEvent $ mkClose k <$ eClose

  pure ()

sharedTable :: Table (RowID :*: Maybe Int :*: Maybe Bool)
(sharedTable, stID :*: stAdd :*: stCount)
  = tableWithSelectors "people"
  $   autoPrimary "id"
  :*: optional "add"
  :*: optional "clear"

data Change =
    CAdd Int
  | CClear
  deriving (Eq, Show)

_State :: Prism' (Maybe Int :*: Maybe Bool) Change
_State =
  let
    toState (CAdd i) = Just i :*: Nothing
    toState CClear = Nothing :*: Just True
    fromState (Just i :*: _) = Right $ CAdd i
    fromState (_ :*: Just _) = Right $ CClear
    fromState t = Left t
  in
    prism toState fromState

applyChange :: Change -> Int -> Int
applyChange (CAdd i) j = i + j
applyChange CClear _ = 0

sharedAll :: MonadSelda m => m [RowID :*: Maybe Int :*: Maybe Bool]
sharedAll =
  query (select sharedTable)

gatherChanges :: [RowID :*: Maybe Int :*: Maybe Bool] -> Int
gatherChanges =
  let
    t (_ :*: xs) = xs
  in
    foldr applyChange 0 .
    reverse .
    mapMaybe (preview _State . t)

-- TODO would be good to use EventWriter to gather exception events
-- - probably with some kind of helper to grab the events up to this point and clear out the list

-- TODO possibly add a snapshot value to the table
-- - add code to update the snapshot and clear the prior values
-- - when do we take care of it? on start up? on join and / or quit of clients?

sharedGuestDB :: (GuestConstraintGroup t m, SeldaEvent t m)
              => Event t (WsData ())
              -> m (Event t ())
sharedGuestDB eInsert = mdo
  ePostBuild <- getPostBuild
  (eError1, eCreated) <- seldaEvent $ tryCreateTable sharedTable <$ ePostBuild
  (eError2, eLoad) <- seldaEvent $ sharedAll <$ eCreated

  dId <- count eInsert

  dModel <- foldDyn ($) Map.empty .
            mergeWith (.) $ [
              Map.insert <$> current dId <@> eInsert
            , flip (foldr Map.delete) <$> eRemoves
            ]

  (_, eWriter) <-
    runEventWriterT . listWithKey dModel $ \k dv -> do
      v <- sample . current $ dv
      acceptSharedDB k dTotal v 

  let
    eRemoves =
      fmap (Set.toList . _sdwClose) eWriter
    eAdd =
      fmap (getSum . _sdwAdd) eWriter
    eClear =
      void . ffilter id . fmap (getAny . _sdwClear) $ eWriter
    eChange =
      leftmost [CClear <$ eClear, CAdd <$> eAdd]

  (eError3, eChangeInserted) <-
    insertEvent sharedTable (\x -> def :*: review _State x) eChange

  dTotal <- foldDyn ($) 0 .
            mergeWith (.) $ [
              const . gatherChanges <$> eLoad
            , applyChange           <$> eChangeInserted
            ]

  performEvent_ $ (liftIO . print) <$> leftmost [eError1, eError2, eError3]

  let
    eEmpty =
      void .
      ffilter Map.null $
      (current dModel <@ eRemoves)

  return eEmpty

data Sources =
  Sources {
    sUnique :: WsManager ()
  , sShared :: WsManager ()
  , sSharedDB :: WsManager ()
  , sBounded :: WsManager ()
  , sIndexed :: WsManager Int
  }

mkSources :: STM Sources
mkSources =
  Sources <$>
    mkWsManager 50 <*>
    mkWsManager 50 <*>
    mkWsManager 50 <*>
    mkWsManager 50 <*>
    mkWsManager 50

indexed :: WsManager Int -> Snap ()
indexed wsm = do
  mi <- getParam "index"
  case mi of
    Nothing ->
      -- TODO proper error handling
      error "boom"
    Just i ->
      -- TODO make sure it reads properly (use readMaybe and then proper error handling)
      wsSnap wsm (read . BC.unpack $ i)

snapApp :: Sources -> String -> Snap ()
snapApp sources baseDir =
  route [
    ("counter/unique", wsSnap (sUnique sources) ())
  , ("counter/shared", wsSnap (sShared sources) ())
  , ("counter/shareddb", wsSnap (sSharedDB sources) ())
  , ("counter/bounded", wsSnap (sBounded sources) ())
  , ("counter/indexed/:index", indexed (sIndexed sources))
  , ("", serveDirectory baseDir)
  ]

wsHost ::
  WsManager a ->
  (forall t m. GuestConstraintGroup t m => Event t (WsData a) -> m b) ->
  IO b
wsHost wsm guest = 
  basicHost $ do
    eWsData <- wsData wsm
    guest eWsData

wsHostDB ::
  WsManager a ->
  (forall t m. (GuestConstraintGroup t m, SeldaEvent t m) => Event t (WsData a) -> m b) ->
  IO b
wsHostDB wsm guest = 
  basicHost $ runSeldaDB (withSQLite "db.sql") $ do
    eWsData <- wsData wsm
    guest eWsData
    -- runSeldaDB (withSQLite "db.sql") (guest eWsData)

go :: String -> IO ()
go baseDir = do
  sources <- atomically mkSources
  eventThreadId1 <- forkIO $ wsHost (sUnique sources) uniqueGuest
  eventThreadId2 <- forkIO $ wsHost (sShared sources) (void . sharedGuest)
  eventThreadId3 <- forkIO $ wsHostDB (sSharedDB sources) (void . sharedGuestDB)
  eventThreadId4 <- forkIO $ wsHost (sBounded sources) (boundedGuest 3)
  eventThreadId5 <- forkIO $ wsHost (sIndexed sources) indexedGuest
  finally (httpServe defaultConfig $ snapApp sources baseDir) $ do
    killThread eventThreadId1
    killThread eventThreadId2
    killThread eventThreadId3
    killThread eventThreadId4
    killThread eventThreadId5
