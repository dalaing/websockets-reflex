{-
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (void, unless, forever, forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Data.IORef (readIORef)
import Data.Maybe (isJust, catMaybes)
import Data.Functor.Identity (Identity(..))
import System.IO (stdin, BufferMode(..), hSetBuffering)

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Primitive (PrimMonad)
import Data.Binary

import Data.Dependent.Sum

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Reflex
import Reflex.Host.Class
import Network.WebSockets

import Reflex.WebSocket
import Reflex.WebSocket.Client

import Commands

guest :: ( Reflex t
         , MonadHold t m
         , TriggerEvent t m
         , PerformEvent t m
         , PostBuild t m
         , MonadIO (Performable m)
         , MonadIO m
         )
      => Connection
      -> Event t String
      -> m (Event t ())
guest conn eLine = do

  let
    eWrite = (pure . LB.toStrict . encode . AddReq . read) <$> eLine
  WebSocket eRead eOpen _ eClose <- webSocket conn $ WebSocketConfig eWrite never

  let
    toTotal (TotalRes i) = Just i

    toTotal _ = Nothing
    eOut = fmapMaybe (toTotal . decode . LB.fromStrict) $ eRead
  performEvent_ $ (liftIO . putStrLn $ "open") <$ eOpen
  performEvent_ $ (\c -> liftIO . putStrLn $ "close " ++ show c) <$> eClose
  -- performEvent_ $ (\l -> liftIO . putStrLn $ "line: " ++ l) <$> eLine
  performEvent_ $ (liftIO . print) <$> eOut

  return $ () <$ eClose

type Guest t m = ( MonadReflexHost t m
                 , MonadHold t m
                 , Ref m ~ Ref IO
                 , MonadRef (HostFrame t)
                 , Ref (HostFrame t) ~ Ref IO
                 , MonadIO (HostFrame t)
                 , PrimMonad (HostFrame t)
                 )
               => Event t String
               -> TriggerEventT t (PostBuildT t (PerformEventT t m)) (Event t ())

host :: (forall t m. Guest t m) -> IO ()
host guest = do
  events <- newChan
  (outLoop, fc) <- runSpiderHost $ do
    (eOpen, eOpenRef) <- newEventWithTriggerRef
    (eLine, eLineRef) <- newEventWithTriggerRef

    (eQuit, fc) <- hostPerformEventT . flip runPostBuildT eOpen . flip runTriggerEventT events . guest $ eLine

    hQuit  <- subscribeEvent eQuit
    liftIO $ hSetBuffering stdin LineBuffering

    let
      readPhase =
        readEvent hQuit >>= sequence
      handleOutputs (Just lmQuit) =
        return $ any isJust lmQuit
      handleOutputs Nothing =
        return False

      loop = do
        input <- liftIO getLine
        mTriggerP <- liftIO $ readIORef eLineRef
        lmQuit <- forM mTriggerP $ \t ->
          runFireCommand fc [t :=> Identity input] readPhase
        quitL <- handleOutputs lmQuit
        unless quitL loop

    mTriggerO <- liftIO . readIORef $ eOpenRef
    lmQuit <- forM mTriggerO $ \t ->
      runFireCommand fc [t :=> Identity ()] readPhase
    quitO <- handleOutputs lmQuit

    let outLoop = unless quitO loop

    return (outLoop, fc)

  void . forkIO . forever $ do
    ers <- readChan events
    runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- runFireCommand fc (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb

  runSpiderHost outLoop

main :: IO ()
main =
  runClient "127.0.0.1" 8000 "/counter/shared" (\c -> host (guest c))
