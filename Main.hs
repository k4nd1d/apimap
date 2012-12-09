module Main (
  main
) where

import System.Environment
import qualified Data.Maybe as M

import Control.Exception (bracket)
import Control.Concurrent (forkIO, threadDelay)
import Data.Acid (AcidState, Update, Query, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)

import qualified ApiMap.ResponseStorage as St
import qualified ApiMap.ClientService as ClientSrv (runApp)
import qualified ApiMap.AdminApiService as AdmApiSrv (runApp)
-- import qualified ApiMap.AdminUiService as AdmUiSrv (runApp)

main :: IO ()
main = do
  args <- getArgs
  let p =
        case args of
          [] -> defaultPort
          pStr:[] -> case maybeRead pStr of
            Just p -> p
            _ -> err
          _ -> err
          where
            err = error "Invalid arguments"
  bracket
    (openLocalState St.initialResponseStorageState)
    createCheckpointAndClose
    (run p)
  where
    run p s = do
      forkIO $ ClientSrv.runApp p s
      forkIO $ AdmApiSrv.runApp (p + 1) s
      infLoop

maybeRead = fmap fst . M.listToMaybe . reads

defaultPort = 8000

infLoop = threadDelay infDelay >> infLoop
infDelay = 60*10^6
