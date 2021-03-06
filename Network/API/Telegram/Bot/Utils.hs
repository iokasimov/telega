module Network.API.Telegram.Bot.Utils ((:*:)((:*:)), field) where

import "aeson" Data.Aeson (ToJSON (toJSON), Object)
import "base" Data.Function ((.))
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (singleton)

infixr 1 :*:

data (:*:) a b = a :*: b

field :: ToJSON a => Text -> a -> Object
field key = singleton key . toJSON
