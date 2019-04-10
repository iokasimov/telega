module Network.Telegram.API.Bot.Object.Message (Message (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.Content (Content)
import Network.Telegram.API.Bot.Object.From (From)

data Message = Direct Int Chat From Content
	deriving Show

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v -> Direct
		<$> v .: "message_id" <*> v .: "chat"
		<*> v .: "from" <*> parseJSON (Object v)
