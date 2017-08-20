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

import qualified Data.Text as T
import qualified Data.Map as M

import Reflex.Dom

import CounterWidget

counterWithClose :: MonadWidget t m 
                 => T.Text 
                 -> T.Text 
                 -> m (Event t ())
counterWithClose url index = el "div" $ mdo
  text index
  counterWidget (T.append url index) eClose
  eClose <- button "Close"
  return eClose

counterListWidget :: MonadWidget t m
                  => T.Text 
                  -> m ()
counterListWidget url = mdo

  tiIndex <- textInput def
  eAddClick <- button "Add"

  let 
    eInsert = 
      (\k -> M.singleton k (Just ())) <$> current (_textInput_value tiIndex) <@ eAddClick
    eDeletes = 
      switch .
      fmap (fmap (Nothing <$) . mergeMap) .
      current $ 
      dMap
    eMap = 
      leftmost [eInsert, eDeletes]

  dMap <- listHoldWithKey M.empty eMap (\k _ -> counterWithClose url k)

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
  el "div" $ 
    counterListWidget "counter/indexed/" 
