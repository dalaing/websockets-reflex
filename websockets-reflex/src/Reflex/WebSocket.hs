{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.WebSocket (
    WebSocketConfig(..)
  , WebSocket(..)
  , webSocket
  ) where

import Control.Monad (void, forever, unless, forM, forM_)
import Control.Concurrent (forkIO, killThread, newChan, readChan)
import Control.Exception (handle)
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, catMaybes)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.TMVar (newEmptyTMVar, tryPutTMVar, tryReadTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue, unGetTQueue)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Network.WebSockets

import Reflex

data WebSocketConfig t =
  WebSocketConfig {
    wscSend :: Event t [B.ByteString]
  , wcsClose :: Event t (Word, B.ByteString)
  }

data WebSocket t =
  WebSocket {
    wsRead :: Event t B.ByteString
  , wsOpen :: Event t ()
  , wsError :: Event t ()
  , wsClose :: Event t (Bool, Word, B.ByteString)
  }

webSocket :: ( MonadHold t m
             , TriggerEvent t m
             , PerformEvent t m
             , PostBuild t m
             , MonadIO (Performable m)
             , MonadIO m
             )
          => Connection
          -> WebSocketConfig t
          -> m (WebSocket t)
webSocket c (WebSocketConfig eSend eCloseIn) = do
  (eRead, triggerERead) <- newTriggerEvent
  currentSocket <- liftIO . atomically . newTVar $ Nothing
  (eOpen, triggerEOpen) <- newTriggerEventWithOnComplete
  (eError, triggerEError) <- newTriggerEvent
  (eCloseOut, triggerECloseOut) <- newTriggerEvent
  payloadQueue <- liftIO . atomically $ newTQueue
  isOpen <- liftIO . atomically $ newEmptyTMVar

  let
    start = liftIO $ do
      atomically . writeTVar currentSocket . Just $ c
      triggerEOpen () $ liftIO . void . atomically . tryPutTMVar isOpen $ ()
      return ()

  ePostBuild <- getPostBuild
  performEvent_ $ start <$ ePostBuild

  performEvent_ $ ffor eSend $ \payloads -> liftIO $ forM_ payloads $
    atomically . writeTQueue payloadQueue

  performEvent_ $ ffor eCloseIn $ \(code, reason) -> liftIO $ do
    mConn <- atomically . readTVar $ currentSocket
    forM_ mConn $ \conn ->
      -- TODO what happens if this throws an exception?
      sendClose conn reason
    triggerECloseOut (True, code, reason)

  readerThreadRef <- liftIO . newIORef $ Nothing
  writerThreadRef <- liftIO . newIORef $ Nothing

  let
    cleanup e = do
      case e of
        CloseRequest w r ->
          triggerECloseOut (True, fromIntegral w, LB.toStrict r)
        _ -> do
          triggerEError ()
          -- TODO more detail here
          triggerECloseOut (False, 1001, "error")

      -- TODO write Nothing to currentSocket and isOpen

      mRId <- readIORef readerThreadRef
      forM_ mRId killThread

      mWId <- readIORef writerThreadRef
      forM_ mWId killThread

  -- need to handle the case where the first recieveData call throws
  -- should prevent the writer thread from starting in the first place

  readerThreadId <- liftIO . forkIO . handle cleanup . forever $ do
    ws <- liftIO . atomically $ do
      mws <- readTVar currentSocket
      case mws of
        Nothing -> retry
        Just ws -> return ws
    msg <- receiveData ws
    triggerERead msg

  liftIO . writeIORef readerThreadRef . Just $ readerThreadId

  writerThreadId <- liftIO . forkIO . handle cleanup . forever $ do
    payload <- atomically $ do
      pl <- readTQueue payloadQueue
      open <- tryReadTMVar isOpen
      if isJust open then return pl else retry

    mws <- liftIO . atomically . readTVar $ currentSocket
    success <- case mws of
      Nothing -> return False
      Just ws -> sendBinaryData ws payload >> return True
    unless success . atomically . unGetTQueue payloadQueue $ payload

  liftIO . writeIORef writerThreadRef . Just $ writerThreadId

  return $
    WebSocket
      eRead
      eOpen
      eError
      eCloseOut

