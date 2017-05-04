{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.WebSocket.Client (
    connect
  , module Reflex.WebSocket
  ) where

import Data.IORef (newIORef, readIORef)

import Network.WebSockets (Connection, runClient)

import Reflex.WebSocket

connect :: String -> Int -> String -> IO Connection
connect host port path =
  readIORef =<< runClient host port path newIORef

