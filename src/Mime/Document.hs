{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Mime.Document where

import Data.Text (Text)
import qualified Data.Text as T

class Doc d where
    (<.>) :: d -> d -> d
    nil :: d
    text :: Text -> d
    line :: d
    nest :: Int -> d -> d
    layout :: d -> Text

instance Doc Text where
    (<.>) = (<>)
    nil = ""
    text = id
    line = T.singleton '\n'

    nest :: Int -> Text -> Text
    nest i = T.replace "\n" ("\n" <> T.replicate i " ")

    layout = id
