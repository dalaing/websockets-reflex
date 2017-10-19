{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module CounterWidget (
    counterWidget
  ) where

import Data.Word
import Text.Read (readMaybe)

import Control.Lens
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (toStrict, fromStrict)

import Reflex.Dom
import Reflex.Dom.WebSocket

import qualified Data.Text as T

import Commands

activeCounterWidgetClosed ::
  MonadWidget t m =>
  Event t Int ->
  m (Event t Int, Event t ())
activeCounterWidgetClosed _ = do
  text "Waiting for websocket..."
  return (never, never)

activeCounterWidgetOpen ::
  MonadWidget t m =>
  Event t Int ->
  m (Event t Int, Event t ())
activeCounterWidgetOpen eTotal = do
  tiAdd <- textInput def
  eAddClick <- button "Add"
  eClear <- button "Clear"

  tiTotal <- textInput $
    def & textInputConfig_setValue .~ fmap (T.pack . show) eTotal

  let
    eAdd =
      fmapMaybe (readMaybe . T.unpack) (current (tiAdd ^. textInput_value) <@ eAddClick)

  return (eAdd, eClear)

counterWidget :: MonadWidget t m
              => T.Text
              -> Event t ()
              -> m ()
counterWidget url eClose = mdo
  let
    acwClosed eTotal = Workflow $ do
      x <- activeCounterWidgetClosed eTotal
      pure (x, acwOpen eTotal   <$ ws ^. webSocket_open)

    acwOpen eTotal = Workflow $ do
      x <- activeCounterWidgetOpen eTotal
      pure (x, acwClosed eTotal <$ ws ^. webSocket_close)

  dpe <- workflow $ acwClosed eTotal

  let
    eAdd   = switch . current . fmap fst $ dpe
    eClear = switch . current . fmap snd $ dpe

  let wsReq = leftmost [
                AddReq   <$> eAdd
              , ClearReq <$  eClear
              ]

  let fullUrl = T.append "ws://127.0.0.1:8000/" url
  ws <- webSocket fullUrl $
    def & webSocketConfig_send  .~ fmap (pure . toStrict . encode) wsReq
        & webSocketConfig_close .~ fmap (const (1000, "bye")) eClose

  let
    wsRes =
      fmap (decode . fromStrict) $ ws ^. webSocket_recv
    toTotalRes (TotalRes i) =
      Just i
    toTotalRes _ =
      Nothing
    eTotal =
      fmapMaybe toTotalRes wsRes

  return ()

