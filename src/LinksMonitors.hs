import Control.Distributed.Process hiding (catch)
import Control.Distributed.Process.Node 
import Control.Concurrent
import Network.Transport hiding (send)
import Network.Transport.InMemory
import Control.Monad.Catch

ex1 :: IO ()
ex1 = do
  t <- liftIO createTransport
  n <- newLocalNode t initRemoteTable
  
  p1 <- forkProcess n $ do
    _ :: ProcessMonitorNotification <- expect
    say "e1!"
  
  p2 <- forkProcess n $ do
    monitor p1
    _ :: ProcessMonitorNotification <- expect
    say "e2!"

  forkProcess n $ kill p1 "bang!"
  
  -- Нічого не робить (бо монітор однонаправлений):
  -- forkProcess n $ kill p1 "bang!"
  
  liftIO (threadDelay (10^6))
  
  closeTransport t


ex2 :: IO ()
ex2 = do
  t <- liftIO createTransport
  n <- newLocalNode t initRemoteTable

  let h (e::SomeException) =
        liftIO $ putStrLn $ "[E] " ++ (show e)
  
  p1 <- forkProcess n $ handle h $ expect >>= say  
  p2 <- forkProcess n $ handle h $ link p1 >> expect >>= say

  forkProcess n $ kill p1 "bang!"

  -- Не вбиває перший процес, бо лінки однонаправлені:
  -- forkProcess n $ do
  --   kill p2 "bang!"
  --   liftIO (threadDelay 1000)
  --   send p1 "I am alive!"
  
  liftIO (threadDelay (10^6))
  
  closeTransport t
