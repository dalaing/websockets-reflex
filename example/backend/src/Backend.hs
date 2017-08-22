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

import Backend.Common
import Backend.Unique
import Backend.Shared
import Backend.SharedDB
import Backend.Bounded
-- import Backend.Indexed

data Sources =
  Sources {
    sUnique :: WsManager ()
  , sShared :: WsManager ()
  , sSharedDB :: WsManager ()
  , sBounded :: WsManager ()
  -- , sIndexed :: WsManager Int
  }

mkSources :: STM Sources
mkSources =
  Sources <$>
--    mkWsManager 50 <*>
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
 -- , ("counter/indexed/:index", indexed (sIndexed sources))
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

go :: String -> IO ()
go baseDir = do
  sources <- atomically mkSources
  eventThreadId1 <- forkIO $ wsHost (sUnique sources) uniqueGuest
  eventThreadId2 <- forkIO $ wsHost (sShared sources) (void . sharedGuest)
  eventThreadId3 <- forkIO $ wsHostDB (sSharedDB sources) (void . sharedGuestDB)
  eventThreadId4 <- forkIO $ wsHost (sBounded sources) (void . boundedGuest 3)
  -- eventThreadId5 <- forkIO $ wsHost (sIndexed sources) (void . indexedGuest)
  finally (httpServe defaultConfig $ snapApp sources baseDir) $ do
    killThread eventThreadId1
    killThread eventThreadId2
    killThread eventThreadId3
    killThread eventThreadId4
    -- killThread eventThreadId5
