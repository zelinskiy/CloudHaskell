{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Distributed.Process
-- import Control.Distributed.Process.Node 
-- import Control.Concurrent
-- import Network.Transport
-- import Network.Transport.InMemory

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data CounterMessage = CounterQuery ProcessId
                    | CounterShutdown
                    | CounterIncrement
                    deriving (Generic, Typeable)

instance Binary CounterMessage

main = putStrLn "OK"
