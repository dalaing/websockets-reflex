{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
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

activeCounterWidget :: MonadWidget t m 
                    => Event t Int
                    -> Bool
                    -> m (Event t Int, Event t ())
activeCounterWidget _ False = do
  text "Waiting for websocket..."
  return (never, never)
activeCounterWidget eTotal True = do
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
  ee <- dyn (activeCounterWidget eTotal <$> dOpen)
  eAdd <- switchPromptly never (fst <$> ee)
  eClear <- switchPromptly never (snd <$> ee)

  let wsReq = leftmost [
                AddReq <$> eAdd
              , ClearReq <$ eClear
              ]

  let fullUrl = T.append "ws://127.0.0.1:8000/" url
  ws <- webSocket fullUrl $
    def & webSocketConfig_send .~ fmap (pure . toStrict . encode) wsReq
        & webSocketConfig_close .~ fmap (const (1000, "bye")) eClose

  dOpen <- holdDyn False . 
           leftmost $ [
             True <$ ws ^. webSocket_open
           , False <$ ws ^. webSocket_close
           ]

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

