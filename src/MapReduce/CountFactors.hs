{-# LANGUAGE TupleSections #-}
module MapReduce.CountFactors
  ( localCountFactors
  , distrCountFactors
  , __remoteTable
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import MapReduce.MapReduce
import MapReduce.PolyDistrMapReduce hiding (__remoteTable)

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = [] 
factorize d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : factorize d (n `div` d)
    | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2

countFactors :: MapReduce Integer Integer () Integer Integer
countFactors = MapReduce {
    mrMap = \k v ->
        map (\n -> ((), toInteger $ length $ primeFactors n)) [k..v-1]
  , mrReduce = const sum
  }


localCountFactors = localMapReduce countFactors

countFactors_ :: () -> MapReduce Integer Integer () Integer Integer
countFactors_ () = countFactors

dictIn :: SerializableDict (Integer, Integer)
dictIn = SerializableDict

dictOut :: SerializableDict [((), Integer)]
dictOut = SerializableDict

remotable ['countFactors_, 'dictIn, 'dictOut]

distrCountFactors :: [NodeId] -> Map Integer Integer -> Process (Map () Integer)
distrCountFactors nodes m =
  distrMapReduce
    $(mkStatic 'dictIn)
    $(mkStatic 'dictOut)
    ($(mkClosure 'countFactors_) ())
    nodes
    (\iteration -> iteration m)



