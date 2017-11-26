import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Data.Map (Map)
import Data.Array (Array, listArray)
import qualified Data.Map as Map (fromList)



import qualified MapReduce.CountWords as CountWords
import qualified MapReduce.MonoDistrMapReduce as MonoDistrMapReduce
import qualified MapReduce.PolyDistrMapReduce as PolyDistrMapReduce
import qualified MapReduce.CountFactors as CountFactors

rtable :: RemoteTable
rtable = MonoDistrMapReduce.__remoteTable
       . PolyDistrMapReduce.__remoteTable
       . CountWords.__remoteTable
       . CountFactors.__remoteTable       
       $ initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    -- Локальне рахування слів
    "local" : "count" : files -> do
      input <- constructInput files
      print $ CountWords.localCountWords input

    -- Розподілений підрахунок слів
    "master" : host : port : "count" : files -> do
      input   <- constructInput files
      backend <- initializeBackend host port rtable
      startMaster backend $ \slaves -> do
        say "Begin"
        result <- CountWords.distrCountWords slaves input
        liftIO $ print $ result

    -- slave
    "slave" : host : port : [] -> do
      backend <- initializeBackend host port rtable
      startSlave backend

    "local" : "factors" : from : to : part : _ -> do
      let input = constructInput2 from to part
      print $ CountFactors.localCountFactors input
      
    "master" : host : port : "factors"
      : from : to : part : _ -> do
      let input = constructInput2 from to part
      backend <- initializeBackend host port rtable
      startMaster backend $ \slaves -> do
        say $ "Begin [" ++ show (length slaves) ++ "]"
        result <- CountFactors.distrCountFactors slaves input
        liftIO $ print $ result
      


constructInput :: [FilePath]
  -> IO (Map FilePath CountWords.Document)
constructInput files = do
  contents <- mapM readFile files
  return . Map.fromList $ zip files contents

arrayFromList :: [e] -> Array Int e
arrayFromList xs = listArray (0, length xs - 1) xs

constructInput2 from to part =
  let (a,b,p) = (read from, read to, read part)
      xs = [a, a+p..b]
  in Map.fromList (zip xs (tail xs))
