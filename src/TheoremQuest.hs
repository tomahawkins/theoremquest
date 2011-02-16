module Main (main) where

import Network.HTTP
import Text.JSON ()

import LoK ()

main :: IO ()
main = simpleHTTP (getRequest "http://localhost:8000") >>= print

