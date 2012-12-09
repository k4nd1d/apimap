{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module ApiMap.AdminApiService (
  runApp
) where

import Yesod
import Yesod.Request
import Network.Wai
import Data.Conduit.Lazy

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import qualified Network.HTTP.Types as HttpTypes

import Data.Maybe
import Control.Monad 
import qualified Data.Text as T

import Control.Applicative ((<$>), (<*>))

import Data.Acid (AcidState, Update, Query)
import Data.Acid.Advanced (query', update')

import qualified ApiMap.ResponseStorage as St
import qualified ApiMap.ResponseStorage.Parse as Prs


data AdminApiService =
    AdminApiService
  { _state :: AcidState St.ResponseStorage
  }

mkYesod "AdminApiService" [parseRoutes|
/setresponse SetResponseR POST
/responsecnt ResponseCntR GET
/responses ResponsesR GET
|]

instance Yesod AdminApiService where


-- |
methodPathKey = BC.pack "methodPath"
responseBodyKey = BC.pack "responseBody"
responseHeadersKey = BC.pack "responseHeaders"

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\(fx, _) -> a == fx)

lookupPostParams' :: B.ByteString -> HttpTypes.Query -> [B.ByteString]
lookupPostParams' key pps = concatMap maybeToList $ lookup' key pps

lookupPostParam' key pps = listToMaybe $ lookupPostParams' key pps

getRequestBody = do
  wr <- waiRequest
  bss <- lift $ lazyConsume $ requestBody wr
  return $ B.concat bss

postParams = HttpTypes.parseQuery
  

getResponseCntR :: Handler RepPlain
getResponseCntR = do
  acid <- fmap _state getYesod
  responseCnt <- query' acid St.GetResponseCnt
  return . RepPlain . toContent $  (T.pack $ show responseCnt)

getResponsesR :: Handler RepPlain
getResponsesR = do
  acid <- fmap _state getYesod
  responses <- query' acid St.GetResponses
  return . RepPlain . toContent $  (T.pack $ show responses)

postSetResponseR :: Handler RepPlain
postSetResponseR = do
  rqBody <- getRequestBody
  let pps = postParams rqBody
  
  let mMethodPath = join $ fmap Prs.parseMethodPath $ lookupPostParam' methodPathKey pps
  let mResponseBody = fmap Prs.mkResponseBody $ lookupPostParam' responseBodyKey pps 
  case (mMethodPath, mResponseBody) of
    (Nothing, _) -> invalidRq
    (_, Nothing) -> invalidRq
    (Just methodPath, Just responseBody) -> do
      let rspHeaders = case lookupPostParam' responseHeadersKey pps of
            Just bs -> Prs.parseResponseHeaders bs
            _ -> St.ResponseHeaders []
      acid <- fmap _state getYesod
      update' acid (St.StoreResponse methodPath responseBody rspHeaders)
      rspUpdated
  where
    invalidRq = return . RepPlain . toContent $ ("invalidSetResponseRq" :: T.Text)
    rspUpdated  = return . RepPlain . toContent $ ("updated" :: T.Text)

errorHandler _ = return . RepPlain . toContent $ ("invalidRq" :: T.Text)

runApp :: Int -> AcidState St.ResponseStorage -> IO ()
runApp port s = warpDebug port (AdminApiService s)
