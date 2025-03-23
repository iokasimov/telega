module Network.API.Telegram.Bot.Utils where

import "aeson" Data.Aeson.Key (fromText)
import "aeson" Data.Aeson.KeyMap (fromList)
import "aeson" Data.Aeson (ToJSON (toJSON), Object, (.=), object)
import "base" Data.Function ((.))
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (singleton)

infixr 1 :*:

data (:*:) a b = a :*: b

field :: ToJSON a => Text -> a -> Object
field key value = fromList [fromText key .= value]
