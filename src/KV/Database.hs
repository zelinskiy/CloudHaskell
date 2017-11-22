{-# LANGUAGE TemplateHaskell #-}

module KV.Database (
  Database,
  Key, Value,
  createDB,
  get, set,
  rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Data.Char
import Data.List
import Text.Printf
import Control.Monad
import Control.Monad.Loops
import Control.Concurrent.STM

import KV.Worker
import KV.Request

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal (database nodes)

set :: Database -> Key -> Value -> Process ()
set db k v = send db (SET k v)

get :: Database -> Key -> Process (Maybe Value)
get db k = do
    (s,r) <- newChan 
    let req = GET k s 
    send db (GET k s)
    receiveChan r 
  
database :: [NodeId] -> Process ()
database nodes = do
    ps <- mapM (\n -> spawn n $(mkStaticClosure 'worker)) nodes

    tps <- liftIO $ newTVarIO (pairs ps)
    spawnLocal $ do 
      mapM_ monitor ps 
      whileM_  (do
                   pids <- liftIO $ atomically $ readTVar tps
                   return $ length pids > 0
               )$
        receiveWait 
        [ match $ \(ProcessMonitorNotification ref deadpid reason) -> do
                    liftIO $ atomically $ modifyTVar' tps $ map (delete deadpid)
                    say $ printf "process %s died: %s" (show deadpid) (show reason)
        ]                      
                   
    forever $ do
      r <- expect :: Process Request
      ps' <- liftIO $ atomically $ readTVar tps
      case r of
        GET k s-> mapM_ (\p -> send p r) (npid k ps') 
        SET k v -> mapM_ (\p -> send p r) (npid k ps') 
    where
        pairs [] = []
        pairs [x] = []
        pairs (x:y:xs) = [x,y] : pairs xs
        npid k ps = ps !! (ord (head k) `mod` length ps)
        

rcdata :: RemoteTable -> RemoteTable
rcdata = KV.Worker.__remoteTable
