{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util (
    listHoldWithKey
  ) where

import qualified Data.Map as M

import Data.Functor.Misc
import Reflex

-- this comes directly from reflex-dom, should it be brought across to reflex (with constraints adjusted)
listHoldWithKey :: ( Ord k
                   , MonadAdjust t m
                   , MonadHold t m
                   )
                => M.Map k v
                -> Event t (M.Map k (Maybe v))
                -> (k -> v -> m a)
                -> m (Dynamic t (M.Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ M.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . M.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a'
