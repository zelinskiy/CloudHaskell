{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node 
import Control.Concurrent
import Network.Transport hiding (send)
import Network.Transport.InMemory
import Data.List
import Control.Monad
import GHC.Generics (Generic)
import Data.Typeable
import Data.Binary
import System.Random

type Philosopher = String
type Fork = Int

data TableInteraction = GrabForks ProcessId (Fork, Fork)
                      | ReleaseForks (Fork, Fork)
                      deriving (Show, Eq, Typeable, Generic)

instance Binary TableInteraction

think :: Philosopher -> Process ()
think p = do
  say $ p ++ " is thinking"
  r <- liftIO $ randomRIO (1,100)
  liftIO $ threadDelay $ r * 100

eat :: Philosopher -> Fork -> Fork -> Process ()
eat p f1 f2 = do
  say $ p ++ " is eating"
  r <- liftIO $ randomRIO (1,100)
  liftIO $ threadDelay $ r * 100

philosopher p = do
  (table, forks) <- expect
  me <- getSelfPid
  loop table forks me
  where
    loop table forks me = do      
      send table $ GrabForks me forks
      maybeForks <- expect
      case maybeForks of
        Nothing -> return ()
        Just (f1, f2) -> do
          eat p f1 f2
          send table $ ReleaseForks forks
      think p
      loop table forks me

table forks = do
  interaction <- expect
  case interaction of
    ReleaseForks (f1, f2) -> table $ f1:f2:forks
    GrabForks pid fs@(f1, f2) -> do
      let (msg, forks') =
            if f1 `elem` forks && f2 `elem` forks
            then (Just fs, forks \\ [f1, f2])
            else (Nothing, forks)
      send pid msg
      table forks'

-- $ stack build && stack exec Main &> out.txt
-- $ grep -o 'Sloterdijk is eating' out.txt | wc -l
-- 416
-- $ grep -o 'Sloterdijk is thinking' out.txt | wc -l
-- 1129

main :: IO ()
main = do
  let philosophers =
        [ "Zizek"
        , "Sloterdijk"
        , "Chalmers"
        , "Dennett"
        , "MacIntyre" ]

  let forks = [1..5]
      forkSets = zip forks (tail . cycle $ forks)
  
  t <- liftIO createTransport
  node <- newLocalNode t initRemoteTable

  table <- forkProcess node $ table forks
  ps <- forM philosophers $  forkProcess node . philosopher

  waiter <- forkProcess node $ do
    forM_ (ps `zip` forkSets) $ \(p, fs) -> send p (table, fs)

  liftIO getLine
  closeTransport t
  

