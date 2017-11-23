import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Data.Map (Map, toList)
import Data.Array (Array, listArray)
import qualified Data.Map as Map (fromList)
import Data.List(sortBy)
import Data.Function (on)

import qualified MapReduce.CountWords as CountWords
import qualified MapReduce.MonoDistrMapReduce as MonoDistrMapReduce

rtable :: RemoteTable
rtable = MonoDistrMapReduce.__remoteTable
       . CountWords.__remoteTable
       $ initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    -- Локальне рахування слів
    "local" : "count" : files -> do
      input <- constructInput files
      print $ take 100 $ sortBy (flip compare `on` snd)
        $ toList $ CountWords.localCountWords input

    -- Розподілений підрахунок слів
    "master" : host : port : "count" : files -> do
      input   <- constructInput files
      backend <- initializeBackend host port rtable
      startMaster backend $ \slaves -> do
        result <- CountWords.distrCountWords slaves input
        liftIO $ print $ take 100 $ sortBy (flip compare `on` snd)
          $ toList $ CountWords.localCountWords input

    -- slave
    "slave" : host : port : [] -> do
      backend <- initializeBackend host port rtable
      startSlave backend


constructInput :: [FilePath]
  -> IO (Map FilePath CountWords.Document)
constructInput files = do
  contents <- mapM readFile files
  return . Map.fromList $ zip files contents

arrayFromList :: [e] -> Array Int e
arrayFromList xs = listArray (0, length xs - 1) xs
