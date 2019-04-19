module Network.API.Telegram.Bot.Object.Member (Member (..), module Exports) where

import Network.API.Telegram.Bot.Object.Member.Powers as Exports
import Network.API.Telegram.Bot.Object.Member.Restrictions as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad (fail, (>>=))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "time" Data.Time.Clock.POSIX (POSIXTime)

import Network.API.Telegram.Bot.Object.Sender (Sender)

data Member
	= Creator Sender
	| Administrator Sender Bool Powers
	| Member Sender
	| Restricted Sender Restrictions POSIXTime
	| Left Sender
	| Kicked Sender POSIXTime
	deriving Show

instance FromJSON Member where
	parseJSON = withObject "Member" $ \v -> v .: "status" >>= \case
		("creator" :: Text) -> Creator <$> v .: "user"
		("administrator" :: Text) -> Administrator <$> v .: "user" <*> v .: "can_be_edited" <*> parseJSON (Object v)
		("member" :: Text) -> Member <$> v .: "user"
		("restricted" :: Text) -> Restricted <$> v .: "user" <*> parseJSON (Object v) <*> v .: "until_date"
		("left" :: Text) -> Left <$> v .: "user"
		("kicked" :: Text) -> Kicked <$> v .: "user" <*> v.: "until_date"
		_ -> fail "Status of chat member is not defined"
