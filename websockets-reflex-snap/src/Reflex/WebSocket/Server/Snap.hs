{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.WebSocket.Server.Snap (
    wsSnap
  , module Reflex.WebSocket.Server
  ) where

import Control.Monad.STM (atomically)

import Snap.Core (Snap)
import Network.WebSockets.Snap (runWebSocketsSnap)

import Reflex.WebSocket.Server
import Reflex.WebSocket.Server.Internal (WsManager, WsData(..), getTicket, sendData, sendDone, waitForDone)

wsSnap :: WsManager a -> a -> Snap ()
wsSnap wsm x =
  runWebSocketsSnap $ \pending -> do
    t <- atomically $ do
      t <- getTicket wsm
      sendData wsm $ WsData (atomically $ sendDone wsm t) pending x
      return t
    atomically $ waitForDone wsm t

