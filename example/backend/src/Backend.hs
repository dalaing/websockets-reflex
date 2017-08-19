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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
module Backend (
    go
  ) where

import Control.Monad (void)

import Control.Monad.Fix (MonadFix)
import Control.Exception (finally)
import Control.Concurrent (forkIO, killThread)

import Control.Monad.STM (STM, atomically)

import Control.Monad.Trans (MonadIO(..))

import Data.Binary (encode, decode)

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M

import Reflex

import Reflex.WebSocket.Server (WsManager(..))
import Reflex.WebSocket.Server.Snap
import Reflex.Basic.Host

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
             -> (M.Map k a -> b)
             -> Dynamic t (M.Map k v)
             -> Event t b
gatherDynMap f g =
  switch .
  fmap (fmap g . mergeMap . fmap f) .
  current

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
uniqueGuest eu = mdo
  bId :: Behavior t Int <- accum (flip ($)) (0 :: Int) (succ <$ eu)

  let
    eInsert =
      (\k -> M.singleton k . Just) <$> bId <@> eu
    eDeletes =
      gatherDynMap uClose (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]

  dMap <- listHoldWithKey M.empty eMap (const acceptUnique)

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
sharedGuest es = mdo
  bId :: Behavior t Int <- accum (flip ($)) (0 :: Int) (succ <$ es)

  let
    eInsert =
      (\k -> M.singleton k . Just) <$> bId <@> es
    eDeletes =
      gatherDynMap sClose (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]

  dMap <- listHoldWithKey M.empty eMap (const $ acceptShared dTotal)

  let
    eAdd =
      gatherDynMap sAdd (sum . M.elems) dMap
    eClear =
      gatherDynMap sClear (() <$) dMap

  dTotal <- foldDyn ($) 0 .
            mergeWith (.) $ [
              (+) <$> eAdd
            , const 0 <$ eClear
            ]

  let
    eEmpty =
      void .
      ffilter M.null $
      (current dMap <@ eDeletes)

  performEvent_ $ (liftIO . putStrLn $ "empty") <$ eEmpty

  return eEmpty

boundedGuest :: GuestConstraintGroup t m
             => Int
             -> Event t (WsData ())
             -> m ()
boundedGuest bound eb = mdo
  bId :: Behavior t Int <- accum (flip ($)) (0 :: Int) (succ <$ eb)

  let
    bHasSpace =
      (\m -> M.size m < bound) <$> current dMap
    eInsert =
      (\k -> M.singleton k . Just) <$> bId <@> gate bHasSpace eb
    eDeletes =
      gatherDynMap sClose (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]

  reject "too many connections" (gate (not <$> bHasSpace) eb)
  dMap <- listHoldWithKey M.empty eMap (const $ acceptShared dTotal)

  let
    eAdd =
      gatherDynMap sAdd (sum . M.elems) dMap
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
      fmap snd . ffilter fst $ (\m k -> (M.notMember k m, k)) <$> current dMap <@> fmap wsPayload ei
    connectsForKey k =
      fmap (() <$) . ffilter ((== k) . wsPayload) $ ei
    eInsert =
      (\k -> M.singleton k . Just $ connectsForKey k) <$> newInserts
    eDeletes =
      gatherDynMap id (Nothing <$) dMap
    eMap =
      leftmost [eInsert, eDeletes]
  dMap <- listHoldWithKey M.empty eMap (const sharedGuest)

  return ()

data Sources =
  Sources {
    sUnique :: WsManager ()
  , sShared :: WsManager ()
  , sBounded :: WsManager ()
  , sIndexed :: WsManager Int
  }

mkSources :: STM Sources
mkSources =
  Sources <$>
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

app :: Sources -> String -> Snap ()
app sources baseDir =
  route [
    ("counter/unique", wsSnap (sUnique sources) ())
  , ("counter/shared", wsSnap (sShared sources) ())
  , ("counter/bounded", wsSnap (sBounded sources) ())
  , ("counter/indexed/:index", indexed (sIndexed sources))
  , ("", serveDirectory baseDir)
  ]

wsHost ::
  WsManager a ->
  (forall t m. (GuestConstraintGroup t m) => Event t (WsData a) -> m b) ->
  IO b
wsHost wsm guest = 
  basicHost $ do
    eWsData <- wsData wsm
    guest eWsData

go :: String -> IO ()
go baseDir = do
  sources <- atomically mkSources
  eventThreadId1 <- forkIO $ wsHost (sUnique sources) uniqueGuest
  eventThreadId2 <- forkIO $ wsHost (sShared sources) (void . sharedGuest)
  eventThreadId3 <- forkIO $ wsHost (sBounded sources) (boundedGuest 3)
  eventThreadId4 <- forkIO $ wsHost (sIndexed sources) indexedGuest
  finally (httpServe defaultConfig $ app sources baseDir) $ do
    killThread eventThreadId1
    killThread eventThreadId2
    killThread eventThreadId3
    killThread eventThreadId4
