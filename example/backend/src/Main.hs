{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main (
    main
  ) where

import System.Environment

import Backend

main :: IO ()
main = do
  baseDir : _ <- getArgs
  go baseDir
