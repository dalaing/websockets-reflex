{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Commands (
    CommandRequest(..)
  , CommandResponse(..)
  ) where

import Data.Word (Word8)

import Data.Binary

data CommandRequest =
    AddReq Int
  | ClearReq
  deriving (Eq, Ord, Show)

instance Binary CommandRequest where
  put (AddReq i) = do
    put (0 :: Word8)
    put i
  put ClearReq =
    put (1 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> AddReq <$> get
      1 -> return ClearReq

data CommandResponse =
    TotalRes Int
  deriving (Eq, Ord, Show)

instance Binary CommandResponse where
  put (TotalRes i) = do
    put (0 :: Word8)
    put i
 
  get = do
    t <- get :: Get Word8
    case t of
      0 -> TotalRes <$> get

