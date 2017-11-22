module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node 
import Control.Concurrent hiding (newChan)
import Network.Transport
import Network.Transport.InMemory 

main :: IO ()
main = do
  t <- liftIO createTransport
  n <- newLocalNode t initRemoteTable

  forkProcess n $ do
    (f1, t1) <- newChan
    (f2, t2) <- newChan

    sendChan f1 "m1"
    sendChan f2 "m2"

    receiveChan t1 >>= say
    receiveChan t2 >>= say

    t3 <- mergePortsBiased [t1, t2]
    -- t3 <- mergePortsRR [t1, t2]

    sendChan f2 "m3"
    sendChan f1 "m4"
    sendChan f1 "m5"

    -- RR: m4 -> m3 -> m5
    -- Biased: m4 -> m5 -> m3
    
    receiveChan t3 >>= say
    receiveChan t3 >>= say
    receiveChan t3 >>= say

  liftIO (threadDelay (10^6))
  closeTransport t


  

