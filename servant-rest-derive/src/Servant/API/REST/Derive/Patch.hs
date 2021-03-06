{-# LANGUAGE OverloadedLists #-}
{-|
Module      : Servant.API.REST.Derive.Patch
Description : Helpers for partial update of data
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.Patch(
    NullablePatch(..)
  , Patchable(..)
  ) where 

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad 
import Data.Aeson
import Data.Monoid 
import Data.Proxy 
import Data.Swagger 
import GHC.Generics 

-- | Special wrapper for those fields that can be nullified at partial update
--
-- Consider you have a data:
-- @
-- data User = User {
--   userName :: Maybe Text
-- }
-- @
--
-- When you define a partial update payload for the 'User' data type, you want
-- to cover use case when a code user desires to nullify contents of 'userName' field.
--
-- The simple approach simply cannot distinguish absence of patch data and nullifying:
-- @
-- data PatchUser = PatchUser {
--   patchUserName :: Maybe Text
-- }
-- @
--
-- So the 'NullablePatch' comes to scene:
-- 
-- @
-- data PatchUser = PatchUser {
--   patchUserName :: NullablePatch Text
-- }
-- @
--
-- >>> encode $ PatchUser NoPatch
-- "{}"
--
-- >>> encode $ PatchUser NullifyPatch
-- "{\"nullify\":true}"
--
-- >>> encode $ PatchUser (ValuePatch "Pupkin")
-- "{\"value\":\"Pupkin\"}"
data NullablePatch a = NoPatch | NullifyPatch | ValuePatch a  
  deriving (Eq, Show, Read, Generic)

instance ToJSON a => ToJSON (NullablePatch a) where 
  toJSON v = case v of 
    NoPatch -> Null
    NullifyPatch -> object ["nullify" .= True]
    ValuePatch a -> object ["value" .= a]

instance FromJSON a => FromJSON (NullablePatch a) where 
  parseJSON (Object o) = nullifyCase 
    <|> (ValuePatch <$> o .: "value")
    where 
    nullifyCase = do 
      b <- o .: "nullify"
      if b then return NullifyPatch
        else mzero
  parseJSON Null = pure NoPatch
  parseJSON _ = mzero

instance ToSchema a => ToSchema (NullablePatch a) where 
  declareNamedSchema _ = do 
    boolSch <- declareSchemaRef (Proxy :: Proxy Bool)
    NamedSchema an as <- declareNamedSchema (Proxy :: Proxy a)
    let nm = ("NullablePatch " <>) <$> an
    return $ NamedSchema nm $ mempty
      & type_ .~ SwaggerObject 
      & properties .~ [
          ("nullify", boolSch)
        , ("value", Inline as)]
      & required .~ []

-- | Defines that 'a' is patchable by type b
class Patchable a b where 
  applyPatch :: a -> b -> a