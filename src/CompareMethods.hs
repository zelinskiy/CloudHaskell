{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Distributed.Process hiding (newChan)
import Control.Distributed.Process.Node 
import Control.Concurrent
import Network.Transport hiding (send)
import Network.Transport.InMemory

import Control.Monad
import Control.Monad.STM
import Control.DeepSeq
import Control.Concurrent.STM

import System.Environment (getArgs)
  
divideList :: Int -> [a] -> [[a]]
divideList n xs = divideList' xs
  where m = (length xs `div` n) + 1
        divideList' [] = []
        divideList' xs = take m xs : divideList' (drop m xs)
        

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = [] 
factorize d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : factorize d (n `div` d)
    | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2

--from = 10^9
--to   = from + 10000
--res  = 823454130942

--from = 10^6
--to   = from + 100
--res  = 11975587

main :: IO ()
main = do
  args <- getArgs
  case args of
    program : nthreads : from : to : [] ->
      let nt = read nthreads
          f = read from
          t = read to
      in case program of
        "0" -> main0 f t
        "1" -> main1 nt f t
        "2" -> main2 nt f t
        "3" -> main3 nt f t
    

-- nthreads = 10000
-- from = 10^9
-- to   = from + 10^5

--Послідовне виконання
main0 from to = print $ sum $ map (sum . primeFactors) [from..to]

--Вбудовані засоби конкурентного програмування
main1 nthreads from to = do
  chan <- newChan
  let works = divideList nthreads [from..to]
  
  forM_ works $ \xs ->
    forkIO $ do
      let work = sum $ map (sum . primeFactors) xs
      work `deepseq` writeChan chan work

  let waitThreads cnt acc = do
        if cnt == length works
          then return acc
          else do
          msg <- readChan chan
          waitThreads (cnt + 1) (msg + acc)
          
  print =<< waitThreads 0 0

--Транзакційна пам'ять
main2 nthreads from to = do  
  acc <- atomically $ newTVar 0
  cnt <- atomically $ newTVar 0
  let works = divideList nthreads [from..to]
  
  forM_ works $
    \xs -> forkIO $ do
      let work = sum (map (sum . primeFactors) xs)
      let write = atomically $ do
            acc0 <- readTVar acc
            cnt0 <- readTVar cnt        
            writeTVar acc (acc0 + work)
            writeTVar cnt (cnt0 + 1)
      work `deepseq` write

  let waitThreads = do
        cnt0 <- atomically $ readTVar cnt
        if cnt0 == length works
          then atomically $ readTVar acc
          else threadDelay 10000 >> waitThreads          
          
  print =<< waitThreads

-- Засоби Cloud Haskell
main3 nthreads from to = do
  t <- liftIO createTransport
  node <- newLocalNode t initRemoteTable
  res <- newEmptyMVar
  let works = divideList nthreads [from..to]
  
  let waitThreads cnt acc = do
        if cnt == length works
          then liftIO $ putMVar res acc
          else do
          msg <- expect :: Process Integer
          waitThreads (cnt + 1) (msg + acc)
      
  master <- forkProcess node (waitThreads 0 0)
  
  forM_ works $ \xs ->
    forkProcess node $ do
        let work = sum (map (sum . primeFactors) xs)
        work `deepseq` send master work

  print =<< takeMVar res
  closeTransport t


  
