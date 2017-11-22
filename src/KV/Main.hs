module Main where

import Control.Distributed.Process
import Control.Monad
import KV.DistribUtils
import KV.Database (createDB, get, set, rcdata)

main = distribMain master rcdata

master :: [NodeId] -> Process ()
master peers = do
  db <- createDB peers
  liftIO $ print peers
  set db "a" "1"
  forever $ do
    liftIO $ putStrLn "ENTER COMMAND:"
    cmd <- liftIO getLine
    case words cmd of
      ["SET", k, v] -> do
        set db k v
      ["GET", k] -> do
        v <- get db k
        liftIO $ putStrLn $ "response: " ++ show v
      _ -> liftIO $ putStrLn $ "usage: SET k v | GET k"

  return ()
