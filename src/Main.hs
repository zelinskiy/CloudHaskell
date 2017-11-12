module Main where

import Control.Distributed.Process hiding (newChan)
import Control.Distributed.Process.Node 
import Control.Concurrent
import Network.Transport hiding (send)
import Network.Transport.InMemory

import Control.Monad
import Control.Monad.STM
import Control.DeepSeq
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Test.QuickCheck

prop_divideList :: Eq a => Positive Int -> [a] -> Bool
prop_divideList (Positive n) xs =
  foldl (++) [] (divideList n xs) == xs

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
main = main2

from = 10^9
to   = from + 10000

--Послідовне виконання
main0 :: IO ()
main0 = print $ sum $ map (sum . primeFactors) [from..to]

--Вбудовані засоби конкурентного програмування
main1 :: IO ()
main1 = do
  chan <- newChan
  
  forM_ [from..to] $ \n ->
    forkIO $ writeChan chan (sum (primeFactors n))

  let waitThreads cnt acc = do
        if cnt == to - from + 1
          then return acc
          else do
          msg <- readChan chan
          waitThreads (cnt + 1) (msg + acc)
          
  print =<< waitThreads 0 0

--Транзакційна пам'ять
main2 :: IO ()
main2 = do
  acc <- atomically $ newTVar 0
  cnt <- atomically $ newTVar 0
  
  forM_ [from..to] $ \n -> forkIO $ atomically $ do
    acc0 <- readTVar acc
    cnt0 <- readTVar cnt
    writeTVar acc (acc0 + sum (primeFactors n))
    writeTVar cnt (cnt0 + 1)

  let waitThreads = do
        cnt0 <- atomically $ readTVar cnt
        if cnt0 == to - from + 1
          then atomically $ readTVar acc
          else threadDelay 100000 >> waitThreads
          
          
  print =<< waitThreads

-- Засоби Cloud Haskell
main3 :: IO ()
main3 = do
  t <- liftIO createTransport
  node <- newLocalNode t initRemoteTable

  done <- newEmptyMVar
  
  let masterWork c res = do
        m <- expect :: Process (Integer, [Integer])
        let res' = m:res
        if c == to - from
          then liftIO $ putMVar done ()
          else masterWork (c+1) res'
      
  master <- forkProcess node (masterWork 0 [])
  
  forM_ [from..to] $ \n ->
    forkProcess node $ do
      send master (n, primeFactors n)

  _ <- takeMVar done
  putStrLn "DONE"
  closeTransport t


  
