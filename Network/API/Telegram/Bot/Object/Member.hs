module Network.API.Telegram.Bot.Object.Member
	(module Exports, Member (..), Ban (..), Kick (..), Unban (..)) where

import Network.API.Telegram.Bot.Object.Member.Powers as Exports
import Network.API.Telegram.Bot.Object.Member.Restrictions as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad (fail, (>>=))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (singleton)

import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint))

data Member
	= Creator Sender
	| Administrator Sender Bool Powers
	| Member Sender
	| Restricted Sender Restrictions Int
	| Left Sender
	| Kicked Sender Int
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

-- | Ban forever or until some date (between 30 seconds and 366 days)
data Ban = Forever | Until Int

data Kick a where
	Kick :: Int64 -> Int -> Ban -> Kick Member

instance Persistable (Kick Member) where
	type Payload (Kick Member) = Kick Member
	payload (Kick chat_id user_id Forever) =
		singleton "chat_id" (toJSON chat_id) <> singleton "user_id" (toJSON user_id)
	payload (Kick chat_id user_id (Until until_date)) = singleton "chat_id" (toJSON chat_id)
		<> singleton "user_id" (toJSON user_id) <> singleton "until_date" (toJSON until_date)
	endpoint _ = "kickChatMember"

data Unban a where
	Unban :: Int64 -> Int -> Unban Member

instance Persistable (Unban Member) where
	type Payload (Unban Member) = Unban Member
	payload (Unban chat_id user_id) =
		singleton "chat_id" (toJSON chat_id)
		<> singleton "user_id" (toJSON user_id)
	endpoint _ = "unbanChatMember"
