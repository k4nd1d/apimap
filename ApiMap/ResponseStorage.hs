{-# LANGUAGE Arrows, NoMonomorphismRestriction, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiMap.ResponseStorage (
  MethodPath(..)
, ResponseBody(..)
, ResponseHeaders(..)
, ResponseStorage(..)
, initialResponseStorageState
, StoreResponse(..)
, FetchResponse(..)
, GetResponseCnt(..)
, GetResponses(..)
) where

import qualified Data.Text as T
import qualified Data.ByteString as B

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Update, Query, makeAcidic)
import Data.Acid.Advanced (update', query')

import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), (@+), (@*), (@<), (@>), (@>=<), (|||), (&&&), size, getOne, ixFun, ixSet, fromList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import qualified Data.ByteString.Char8 as BC

data MethodPath =
   MethodPath
 { _mpText :: T.Text
 }
 deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''MethodPath)

data ResponseBody =
   ResponseBody
 { _rbBS :: B.ByteString
 }
 deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ResponseBody)

data ResponseHeaders =
   ResponseHeaders
 { _kvPairs :: [(T.Text, T.Text)]
 }
 deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ResponseHeaders)

data Method =
   Method
 { _mPath :: MethodPath
 , _mrHeaders :: ResponseHeaders
 , _mrBody :: ResponseBody
 }
 deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Method)

instance Indexable Method where
 empty = ixSet [ ixFun $ \m -> [ _mPath m ]
               ]

data ResponseStorage =
   ResponseStorage
 { _methods :: IxSet Method
 }
 deriving (Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ResponseStorage)

initialResponseStorageState :: ResponseStorage
initialResponseStorageState =
 ResponseStorage
 { _methods = empty
 }


setMethods m (ResponseStorage _) = ResponseStorage m

storeResponse :: MethodPath -> ResponseBody -> ResponseHeaders -> Update ResponseStorage ()
storeResponse path body hdrs = do
  s <- get
  put $ setMethods (IxSet.updateIx path (Method path hdrs body) (_methods s)) s
  return ()

fetchResponse :: MethodPath -> Query ResponseStorage (Maybe (ResponseHeaders, ResponseBody))
fetchResponse path = do
  s <- ask
  case IxSet.getOne $ (_methods s) @= path of
    Just (Method _ hs b) -> return $ Just (hs, b)
    _ -> return Nothing

getResponseCnt :: Query ResponseStorage (Int)
getResponseCnt = do
  s <- ask
  return $ IxSet.size (_methods s)

getResponses :: Query ResponseStorage [Method]
getResponses = do
  s <- ask
  return $ IxSet.toList (_methods s)

$(makeAcidic ''ResponseStorage
 [ 'getResponseCnt
 , 'storeResponse
 , 'fetchResponse
 , 'getResponses
 ])

