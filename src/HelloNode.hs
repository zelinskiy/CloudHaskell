import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent
import Network.Transport
import Network.Transport.TCP



main :: IO ()
main = do
  maybeTransport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
  let t = case maybeTransport of
            Left _ ->
              error "Can't create TCP channel"
            Right t -> t
  
  onode <- newLocalNode t initRemoteTable
  dnode <- newLocalNode t initRemoteTable
  
  desdemona <- forkProcess dnode $ do
    question <- expect :: Process String
    say "Ay, my lord."
    die "A guiltless death I die."
    
  forkProcess onode $ do
    say "Have you pray'd to-night, Desdemona?"
    send desdemona "a question"
    die "Killing myself, to die upon a kiss."

  liftIO $ threadDelay (10^5)
  closeTransport t
  liftIO $ putStrLn "DONE"
  
