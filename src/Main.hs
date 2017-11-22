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

type Philosopher = String
type Fork = Int

data TableInteraction = GrabForks ProcessId (Fork, Fork)
                      | ReleaseForks ProcessId (Fork, Fork)
                      deriving (Show, Eq, Typeable, Generic)

instance Binary TableInteraction

think :: Philosopher -> Process ()
think p = do
  say $ p ++ " is thinking"
  liftIO $ threadDelay 1000

eat :: Philosopher -> Fork -> Fork -> Process (Fork, Fork)
eat p f1 f2 = do
  say $ p ++ " is eating"
  liftIO $ threadDelay 100
  return (f1, f2)

philosopher p = do
  --say $ "spawned " ++ p
  (table, forks) <- expect
  --say $ p ++ "'s forks are " ++ show forks
  me <- getSelfPid
  loop table forks me
  where
    loop table forks me = do
      -- say $ p ++ " tries to grab his forks"
      send table $ GrabForks me forks
      maybeForks <- expect
      case maybeForks of
        Nothing -> do
          --say $ p ++ " can't get his forks"
          think p
        Just (f1, f2) -> do
          --say $ p ++ " got his forks"
          eat p f1 f2
          send table $ ReleaseForks me forks
      loop table forks me

table forks = do
  interaction <- expect
  case interaction of
    ReleaseForks pid (f1, f2) -> table $ f1:f2:forks
    GrabForks pid fs@(f1, f2) -> do
      send pid $ if (f1 `elem` forks && f2 `elem` forks)
                 then Just fs else Nothing
      
      table $ forks \\ [f1, f2]

-- $ stack build && stack exec Main &> out.txt
-- $ grep -o 'Zizek' out.txt | wc -l
-- 18847
-- $ grep -o 'Sloterdijk' out.txt | wc -l
-- 18274
-- $ grep -o 'Chalmers' out.txt | wc -l
-- 18274
-- $ grep -o 'Dennett' out.txt | wc -l
-- 18279
-- $ grep -o 'MacIntyre' out.txt | wc -l
-- 18276


main :: IO ()
main = do
  let philosophers =
        [ "Zizek"
        , "Sloterdijk"
        , "Chalmers"
        , "Dennett"
        , "MacIntyre" ]

  let forks = [1..5]
      forkPairs = zip forks (tail . cycle $ forks)
  
  t <- liftIO createTransport
  node <- newLocalNode t initRemoteTable

  table <- forkProcess node $ table forks
  ps <- forM philosophers $  forkProcess node . philosopher

  waiter <- forkProcess node $ do
    forM_ (ps `zip` forkPairs) $ \(p, fs) -> send p (table, fs)

  liftIO getLine
  closeTransport t
  

