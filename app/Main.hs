module Main (main) where

import RIO
import qualified Text.Pandoc.Md2Redmine

main :: IO ()
main = Text.Pandoc.Md2Redmine.run
