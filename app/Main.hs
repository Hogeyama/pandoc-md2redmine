module Main (main) where

import RIO
import qualified RIO.ByteString as B
import qualified Text.Pandoc.Md2Redmine

main :: IO ()
main = B.putStr . encodeUtf8 =<< Text.Pandoc.Md2Redmine.run
