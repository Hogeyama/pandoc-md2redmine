module Main (main) where

import qualified MyLib (someFunc)
import RIO

main :: IO ()
main = do
  hPutBuilder stdout "Hello, Haskell!"
  MyLib.someFunc
