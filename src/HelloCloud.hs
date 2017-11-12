import Control.Distributed.Process
import Control.Distributed.Process.Node 
import Control.Concurrent
import Network.Transport
import Network.Transport.InMemory 

main :: IO ()
main = do
  -- Створимо новий транспорт
  t <- liftIO createTransport

  -- Створимо нову локальну ноду
  n <- newLocalNode t initRemoteTable

  -- Запустимо на ньому процес
  forkProcess n $ do
    say "Hello, Haskell!"

  -- Чекаємо секунду, аби процес встиг
  liftIO (threadDelay (10^6))

  -- Акуратно закриваємо транспорт
  closeTransport t
  
