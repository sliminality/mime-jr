{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Mime.Document (Doc (..), IntoDoc (..)) where

import Data.Text (Text)
import qualified Data.Text as T

class Doc d where
    (<.>) :: d -> d -> d
    nil :: d
    text :: Text -> d
    line :: d
    nest :: Int -> d -> d
    layout :: d -> Text
    len :: d -> Int
    
instance Doc Text where
    (<.>) = (<>)
    nil = ""
    text = id
    line = T.singleton '\n'

    nest :: Int -> Text -> Text
    nest i = T.replace "\n" ("\n" <> T.replicate i " ")

    layout = id

    len = T.length

class IntoDoc a where
    repr :: (Doc d) => a -> d

    -- | Returns the total formatted width of the entity.
    width :: a -> Int
