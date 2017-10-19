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

import Snap.Core (Snap)
import Network.WebSockets (PendingConnection)
import Network.WebSockets.Snap (runWebSocketsSnap)

import Reflex.WebSocket.Server (WsManager, handleConnection)

wsSnap :: 
  WsManager PendingConnection -> 
  Snap ()
wsSnap wsm =
  runWebSocketsSnap (handleConnection wsm)

