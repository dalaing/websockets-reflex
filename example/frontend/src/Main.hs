{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
  ) where

import Control.Lens (view)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom

import CounterWidget

counterWithClose :: MonadWidget t m
                 => Text
                 -> Text
                 -> EventWriterT t (Set Text) m ()
counterWithClose url index = el "div" $ mdo
  text index
  counterWidget (Text.append url index) eClose
  eClose <- button "Close"
  tellEvent $ Set.singleton index <$ eClose

counterListWidget :: MonadWidget t m
                  => Text
                  -> m ()
counterListWidget url = mdo

  tiIndex <- textInput def
  eAddClick <- button "Add"

  dModel <- foldDyn ($) Map.empty .
            mergeWith (.) $ [
              Map.insert <$> current (view textInput_value tiIndex) <@> eAddClick
            , flip (foldr Map.delete) <$> eRemoves
            ]

  (_, eRemoves) <- runEventWriterT .
                   listWithKey dModel $ \k _ ->
                     counterWithClose url k

  return ()

main :: IO ()
main = mainWidget $ el "div" $ do
  el "div" $
    counterWidget "counter/unique" never
  el "div" $
    counterWidget "counter/shared" never
  el "div" $
    counterWidget "counter/shareddb" never
  el "div" $
    counterWidget "counter/bounded" never
  -- el "div" $
    -- counterListWidget "counter/indexed/"
