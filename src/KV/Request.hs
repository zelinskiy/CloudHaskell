{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module KV.Request where

import Control.Distributed.Process
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics


type Key   = String
type Value = String

data Request = GET Key (SendPort (Maybe Value))
             | SET Key Value
             deriving (Typeable,Generic)

instance Binary Request
