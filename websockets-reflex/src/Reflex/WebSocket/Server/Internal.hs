{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.WebSocket.Server.Internal where

import Control.Monad (void, forever, forM_, forM)
import Control.Monad.Fix (MonadFix)
import Control.Concurrent (forkIO)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)

import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue)
import qualified Control.Concurrent.STM.Map as SM
import Control.Monad.Trans (MonadIO(..))
import Data.Hashable (Hashable(..))

import Data.ByteString as B

import Network.WebSockets (PendingConnection, Connection, rejectRequest, acceptRequest)

import Reflex
import Reflex.TriggerEvent.Class

import Reflex.WebSocket

newtype Ticket = Ticket Int
  deriving (Eq, Ord, Show, Hashable)

data WsData a =
  WsData {
    wsDone :: IO ()
  , wsConnection :: a
  }

data WsManager a =
  WsManager (TVar Ticket) (TBQueue (WsData a)) (SM.Map Ticket ())

mkWsManager :: Int -> STM (WsManager a)
mkWsManager size =
  WsManager <$> newTVar (Ticket 0) <*> newTBQueue size <*> SM.empty

getTicket :: WsManager a -> STM Ticket
getTicket (WsManager tv _ _) = do
  Ticket t <- readTVar tv
  writeTVar tv $ Ticket (succ t)
  return $ Ticket t

sendData :: WsManager a -> WsData a -> STM ()
sendData (WsManager _ q _) =
  writeTBQueue q

waitForData :: WsManager a -> STM (WsData a)
waitForData (WsManager _ q _) =
  readTBQueue q

sendDone :: WsManager a -> Ticket -> STM ()
sendDone (WsManager _ _ m) t =
  SM.insert t () m

waitForDone :: WsManager a -> Ticket -> STM ()
waitForDone (WsManager _ _ m) t = do
  v <- SM.lookup t m
  case v of
    Just x -> do
      SM.delete t m
      return x
    Nothing ->
      retry

reject :: ( PerformEvent t m
          , MonadIO (Performable m)
          )
       => Behavior t B.ByteString
       -> Event t (WsData PendingConnection)
       -> m ()
reject b e =
  let
    go r w = liftIO $ do
      rejectRequest (wsConnection w) r
      wsDone w
  in
    performEvent_ $ go <$> b <@> e

accept :: ( MonadHold t m
          , TriggerEvent t m
          , PerformEvent t m
          , PostBuild t m
          , MonadIO (Performable m)
          , MonadIO m
          , MonadFix m
          )
       => WsData PendingConnection
       -> WebSocketConfig t
       -> Event t ()
       -> m (WebSocket t)
accept (WsData done pending) wsc eDone = do
  conn <- liftIO $ acceptRequest pending
  ws <- webSocket conn wsc
  performEvent_ $ liftIO done <$ eDone
  return ws

connect :: ( MonadHold t m
           , TriggerEvent t m
           , PerformEvent t m
           , PostBuild t m
           , MonadIO (Performable m)
           , MonadIO m
           , MonadFix m
           )
        => WsData Connection
        -> WebSocketConfig t
        -> m (WebSocket t)
connect (WsData done conn) wsc = do
  ws <- webSocket conn wsc
  performEvent_ $ liftIO done <$ wsClosed ws
  return ws

wsData ::
  ( TriggerEvent t m
  , MonadIO m
  ) =>
  WsManager a ->
  m (Event t (WsData a))
wsData wsm = do
  (eData, onData) <- newTriggerEvent

  void . liftIO . forkIO . forever $ do
    wsd <- atomically $ waitForData wsm
    onData wsd

  pure eData

handleConnection ::
  WsManager a ->
  a ->
  IO ()
handleConnection wsm a = do
  t <- atomically $ do
    t <- getTicket wsm
    sendData wsm $ WsData (atomically $ sendDone wsm t) a
    return t
  atomically $ waitForDone wsm t
