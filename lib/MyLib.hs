module MyLib (run) where

import RIO

-- | Some function
--
-- >>> run
run :: IO ()
run = hPutBuilder stdout "someFunc"
