{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Backend.Common (
    GuestConstraintSingle
  , GuestConstraintGroup
  , applyRequest
  , foldRequest
  , foldRequestE
  , decodeRequest
  , encodeResponse
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadIO)

import Data.Binary (encode, decode)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Reflex

import Commands

type GuestConstraintSingle t m =
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , PostBuild t m
  , MonadIO (Performable m)
  , MonadIO m
  )

type GuestConstraintGroup t m =
  ( GuestConstraintSingle t m
  , MonadAdjust t m
  )

applyRequest ::
  CommandRequest ->
  Int ->
  Int
applyRequest (AddReq i) j =
  i + j
applyRequest ClearReq _ =
  0

foldRequest ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Int ->
  Event t CommandRequest ->
  m (Dynamic t Int)
foldRequest i e =
  foldDyn ($) i (applyRequest <$> e)

foldRequestE ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Event t Int ->
  Event t CommandRequest ->
  m (Dynamic t Int)
foldRequestE i e =
  foldDyn ($) 0 .
  mergeWith (.) $ [
    const <$> i
  , applyRequest <$> e
  ]

decodeRequest ::
  Reflex t =>
  Event t B.ByteString ->
  Event t CommandRequest
decodeRequest =
  fmap (decode . LB.fromStrict)

encodeResponse ::
  Reflex t =>
  Event t CommandResponse ->
  Event t [B.ByteString]
encodeResponse =
  fmap (pure . LB.toStrict . encode)
