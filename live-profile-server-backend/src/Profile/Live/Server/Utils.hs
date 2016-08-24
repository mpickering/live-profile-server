{-|
Module      : Profile.Live.Server.Utils
Description : Generic helpers
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Utils(
    showt
  , whenNothing
  , fromKey
  , toKey
  ) where

import Database.Persist.Sql 
import Data.Text (Text)

import qualified Data.Text as T  

-- | Shortcut 'pack . show'
showt :: Show a => a -> Text 
showt = T.pack . show 

-- | Run action if 'Maybe' is 'Nothing'
whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing ma f = case ma of 
  Nothing -> f 
  Just _ -> pure ()

-- | Shortcut to convert sql key
fromKey :: (Integral a, ToBackendKey SqlBackend record) 
  => Key record -> a 
fromKey = fromIntegral . fromSqlKey

-- | Shortcut to convert sql key
toKey :: (Integral a, ToBackendKey SqlBackend record) 
  => a -> Key record 
toKey = toSqlKey . fromIntegral