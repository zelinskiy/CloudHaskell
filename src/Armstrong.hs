{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

import System.Environment (getArgs)
import Control.Distributed.Process
-- import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import Data.Typeable
import GHC.Generics
import Data.Binary

data Command = Increment
             | Get ProcessId
             | Suicide
             deriving (Typeable, Generic)

instance Binary Command

master :: Backend -> Process ()
master backend = do
  pid <- spawnLocal logCounter
  run pid
  where
    usage = "Usage: I(ncrement) | G(et) | S(uicide)"
    
    parse i "G" = Just (Get i)
    parse _ "I" = Just Increment
    parse _ "S" = Just Suicide
    parse _  _  = Nothing

    logCounter = do
      (from, n) :: (ProcessId, Int) <- expect
      liftIO $ putStrLn $ "{" ++ show from ++ " = " ++ show n ++ "}"
      logCounter
      
    run pid = do
      slaves <- findSlaves backend
      liftIO $ putStrLn $ "[" ++ show (length slaves) ++ "] Enter message: "
      msg <- liftIO $ getLine
      case parse pid msg of
        Nothing -> do liftIO $ putStrLn usage
        Just cmd -> forM_ slaves $ flip send cmd
      run pid
      
slave :: Process ()
slave = do
  me <- getSelfPid
  register "slaveController" me
  go me (0 :: Int)
  where go me n = do
          cmd :: Command <- expect
          n' <- case cmd of
            Increment -> return (n + 1)
            Suicide -> die Suicide
            Get pid -> send pid (me, n) >> return n
          liftIO $ putStrLn $ "[" ++ show n' ++ "]"
          go me n'

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      node <- newLocalNode backend
      runProcess node (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      node <- newLocalNode backend
      runProcess node slave
      
