{-# LANGUAGE OverloadedStrings #-}


module ApiMap.ResponseStorage.Parse (
  parseMethodPath
, mkResponseBody  
, parseResponseHeaders
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Maybe as M

import qualified Control.Exception as X

import qualified ApiMap.ResponseStorage as St

{-
methodPath=qwe/xcv/fgh
responseBody="{\"custom\": \"response\",\n\"body\": \"text_value\"\n}"
responseHeaders="[(\"X-Header-Name1\", \"val1\"), (\"X-Header-Name2\", \"val2\")]"
-}

parseMethodPath :: B.ByteString -> Maybe St.MethodPath
parseMethodPath bs = Just $ St.MethodPath $ TE.decodeUtf8 bs

mkResponseBody :: B.ByteString -> St.ResponseBody
mkResponseBody bs = St.ResponseBody bs

parseResponseHeaders :: B.ByteString -> St.ResponseHeaders
parseResponseHeaders bs = case maybeRead $ BC.unpack bs of
  Just hs -> St.ResponseHeaders hs
  _ -> St.ResponseHeaders []

maybeRead = fmap fst . M.listToMaybe . reads
