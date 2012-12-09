{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module ApiMap.ClientService (
  runApp
) where

import Yesod
import Data.Text
import Data.List
import qualified Data.Text as T

import Data.Acid (AcidState, Update, Query)
import Data.Acid.Advanced (query', update')

import qualified ApiMap.ResponseStorage as St



data ClientService =
    ClientService
  { _state :: AcidState St.ResponseStorage
  }

data MethodPieces =
  MethodPieces
  { _pieces :: [Text]
  }
  deriving (Eq, Show, Read)

instance PathMultiPiece MethodPieces where
  toPathMultiPiece (MethodPieces ts) = ts
  fromPathMultiPiece ts = Just $ MethodPieces ts

mkYesod "ClientService" [parseRoutes|
/*MethodPieces ResponseR
|]

instance Yesod ClientService

defaultResponse = (RepPlain . toContent) ("nothing" :: T.Text)

handleResponseR :: MethodPieces -> Handler RepPlain
handleResponseR methodPieces = do
  acid <- fmap _state getYesod
  let path = T.intercalate "/" $ _pieces methodPieces
  mHsB <- query' acid (St.FetchResponse $ St.MethodPath path)
  case mHsB of
    Just (St.ResponseHeaders hs, St.ResponseBody b) -> do
      mapM_ (\(k,v) -> setHeader k v) hs
      return $ (RepPlain . toContent) b
    _ -> return $ defaultResponse


runApp :: Int -> AcidState St.ResponseStorage -> IO ()
runApp port s = warpDebug port (ClientService s)
