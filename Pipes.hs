module Pipes
    (
      Pipes
    , initPipes
    , addShellPipe
    , addSSHPipe
    , push
    , pushMap
    ) where

{-
This is a preliminary attempt!
There is hardly any abstraction.
-}

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.ParallelIO.Global (parallel)
import qualified Data.Map as M
import System.IO
import System.Process (runInteractiveCommand)

--use existential type? so I can have Handle and any other thing
data Pipes = Pipes
    {
      pipes :: TChan (Handle,Handle)
    }

initPipes :: IO Pipes
initPipes = do
  chan <- newTChanIO
  return $ Pipes chan 

addShellPipe :: Pipes -> String -> IO ()
addShellPipe ps cmd = do
  (inp,out,_,_) <- runInteractiveCommand cmd
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  addPipe ps (inp,out)

addSSHPipe :: Pipes -> String -> String -> IO ()
addSSHPipe ps server cmd =
  addShellPipe ps $ "ssh " ++ server ++ " '" ++ cmd ++ "'"

addPipe ps handles =
  atomically $ writeTChan (pipes ps) handles

push :: Pipes -> (Handle -> IO a) -> String -> IO a
push ps handler input = do
  handles@(inp,out) <- atomically $ readTChan (pipes ps)
  hPutStr inp input
  result <- handler out
  atomically $ writeTChan (pipes ps) handles
  return result

pushMap :: Pipes -> (Handle -> IO a) -> [String] -> IO [a]
pushMap ps handler = parallel.map (push ps handler)
