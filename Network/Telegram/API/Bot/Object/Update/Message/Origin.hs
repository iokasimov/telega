module Network.Telegram.API.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Update.Message.From (From)

data Origin
	= Private Int64 From
	| Group Int64 Text From
	| Supergroup Int64 Text From
	| Channel Int64 Text
	deriving Show

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \v -> v .: "chat" >>= chat v where

		chat :: Object -> Value -> Parser Origin
		chat v = withObject "Origin" $ \c -> c .: "type" >>= \case
			("private" :: Text) -> Private <$> c .: "id" <*> v .: "from"
			("group" :: Text) -> Group <$> c .: "id" <*> c .: "title" <*> v .: "from"
			("supergroup" :: Text) -> Supergroup <$> c .: "id" <*> c .: "title" <*> v .: "from"
			("channel" :: Text) -> Channel <$> c .: "id" <*> c .: "title"
			_ -> fail "Type of chat is not defined"
