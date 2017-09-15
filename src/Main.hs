
module Main where

import Rwm.Rwm (launch)
import Control.Exception (SomeException, displayException, catch)

main = launch `catch` rescue where
  rescue :: SomeException → IO ()
  rescue = putStrLn . displayException

