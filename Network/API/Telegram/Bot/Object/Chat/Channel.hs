module Network.API.Telegram.Bot.Object.Chat.Channel (Channel (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Channel = Channel Int64 Title deriving Show

instance Identifiable Channel where
	type Identificator Channel = Int64
	ident (Channel i _) = i

instance FromJSON Channel where
	parseJSON = withObject "Channel" $ \chat -> chat .: "type" >>= \case
		("channel" :: Text) -> Channel <$> chat .: "id" <*> chat .: "title"
		_ -> fail "Not a channel chat!"
