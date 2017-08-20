{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util (
    listHoldWithKey
  , listWithKey
  ) where

import Control.Monad.Fix (MonadFix)

import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex
import Data.Functor.Misc
import Data.Align
import Data.These

-- this comes directly from reflex-dom, should it be brought across to reflex (with constraints adjusted)

mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
  where (ls, rs) = Map.partition isLeft m
        fromLeft (Left l) = l
        fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
        fromRight (Right r) = r
        fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
        maybeToEither = \case
          Nothing -> Left ()
          Just r -> Right r

listHoldWithKey :: ( Ord k
                   , MonadAdjust t m
                   , MonadHold t m
                   )
                => Map k v
                -> Event t (Map k (Maybe v))
                -> (k -> v -> m a)
                -> m (Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a'

listWithKey :: forall t k v m a. (Ord k, MonadAdjust t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  postBuild <- getPostBuild
  let childValChangedSelector = fanMap $ updated vals
      -- We keep track of changes to children values in the mkChild function we pass to listHoldWithKey
      -- The other changes we need to keep track of are child insertions and deletions. diffOnlyKeyChanges
      -- keeps track of insertions and deletions but ignores value changes, since they're already accounted for.
      diffOnlyKeyChanges olds news = flip Map.mapMaybe (align olds news) $ \case
        This _ -> Just Nothing
        These _ _ -> Nothing
        That new -> Just $ Just new
  rec sentVals :: Dynamic t (Map k v) <- foldDyn applyMap Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffOnlyKeyChanges (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild --TODO: This should probably be added to the attachWith, not to the updated; if we were using diffMap instead of diffMapNoEq, I think it might not work
                         ]
  listHoldWithKey Map.empty changeVals $ \k v ->
    mkChild k =<< holdDyn v (select childValChangedSelector $ Const2 k)

