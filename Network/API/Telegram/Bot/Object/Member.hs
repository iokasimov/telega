module Network.API.Telegram.Bot.Object.Member
	( module Exports, Member (..), Until (..), Can (..), Cannot (..)
	, Kick (..), Unban (..), Restrict (..), Promote (..)) where

import Network.API.Telegram.Bot.Object.Member.Powers as Exports
import Network.API.Telegram.Bot.Object.Member.Restrictions as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad (fail, (>>=))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show)
import "data-default" Data.Default (Default (def))
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Chat (Chat)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident), ID)
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, Returning, payload, endpoint))
import Network.API.Telegram.Bot.Utils (field)

data Member
	= Creator Sender
	| Administrator Sender Bool Powers
	| Member Sender
	| Restricted Sender Restrictions Int
	| Left Sender
	| Kicked Sender Int
	deriving Show

instance Accessible Sender Member where
	access f (Creator sender) = (\sender' -> Creator sender') <$> f sender
	access f (Administrator sender cbe powers) = (\sender' -> Administrator sender' cbe powers) <$> f sender
	access f (Member sender) = (\sender' -> Member sender') <$> f sender
	access f (Restricted sender restrictions until) = (\sender' -> Restricted sender' restrictions until) <$> f sender
	access f (Left sender) = (\sender' -> Left sender') <$> f sender
	access f (Kicked sender until) = (\sender' -> Kicked sender' until) <$> f sender

instance Identifiable Member where
	type Identificator Member = Sender
	ident (Creator sender) = sender
	ident (Administrator sender _ _) = sender
	ident (Member sender) = sender
	ident (Restricted sender _ _) = sender
	ident (Left sender) = sender
	ident (Kicked sender _) = sender

instance FromJSON Member where
	parseJSON = withObject "Member" $ \v -> v .: "status" >>= \case
		("creator" :: Text) -> Creator <$> v .: "user"
		("administrator" :: Text) -> Administrator <$> v .: "user" <*> v .: "can_be_edited" <*> parseJSON (Object v)
		("member" :: Text) -> Member <$> v .: "user"
		("restricted" :: Text) -> Restricted <$> v .: "user" <*> parseJSON (Object v) <*> v .: "until_date"
		("left" :: Text) -> Left <$> v .: "user"
		("kicked" :: Text) -> Kicked <$> v .: "user" <*> v.: "until_date"
		_ -> fail "Status of chat member is not defined"

newtype Can a = Can a

instance Default (Can Restrictions) where
	def = Can $ Restrictions True True True True

instance Default (Can Powers) where
	def = Can $ Powers True True True True True True True True

newtype Cannot a = Cannot a

instance Default (Cannot Restrictions) where
	def = Cannot $ Restrictions False False False False

instance Default (Cannot Powers) where
	def = Cannot $ Powers False False False False False False False False

-- | Ban forever or until some date (between 30 seconds and 366 days)
data Until = Forever | Until Int

data Kick a where
	Kick :: ID Chat -> ID Sender -> Until -> Kick Member

instance Persistable (Kick Member) where
	type Payload (Kick Member) = Kick Member
	type Returning (Kick Member) = ()
	payload (Kick chat_id user_id Forever) =
		field "chat_id" chat_id <> field "user_id" user_id
	payload (Kick chat_id user_id (Until until_date)) = field "chat_id" chat_id
		<> field "user_id" user_id <> field "until_date" until_date
	endpoint _ = "kickChatMember"

data Unban a where
	Unban :: ID Chat -> ID Sender -> Unban Member

instance Persistable (Unban Member) where
	type Payload (Unban Member) = Unban Member
	type Returning (Unban Member) = ()
	payload (Unban chat_id user_id) =
		field "chat_id" chat_id
		<> field "user_id" user_id
	endpoint _ = "unbanChatMember"

data Restrict a where
	Restrict :: ID Chat -> ID Sender -> Until -> Restrictions -> Restrict Member

instance Persistable (Restrict Member) where
	type Payload (Restrict Member) = Restrict Member
	type Returning (Restrict Member) = ()
	payload (Restrict chat_id user_id Forever (Restrictions send_msgs send_media_msgs send_other_msgs wp_previews)) =
		field "chat_id" chat_id <> field "user_id" user_id
		<> field "can_send_messages" send_msgs <> field "can_send_media_messages" send_media_msgs
		<> field "can_send_other_messages" send_other_msgs <> field "can_add_web_page_previews" wp_previews
	payload (Restrict chat_id user_id (Until until_date) (Restrictions send_msgs send_media_msgs send_other_msgs wp_previews)) =
		field "chat_id" chat_id <> field "user_id" user_id <> field "until_date" until_date
		<> field "can_send_messages" send_msgs <> field "can_send_media_messages" send_media_msgs
		<> field "can_send_other_messages" send_other_msgs <> field "can_add_web_page_previews" wp_previews
	endpoint _ = "restrictChatMember"

data Promote a where
	Promote :: ID Chat -> ID Sender -> Until -> Powers -> Promote Member

instance Persistable (Promote Member) where
	type Payload (Promote Member) = Promote Member
	type Returning (Promote Member) = ()
	payload (Promote chat_id user_id Forever (Powers change_info post_msgs edit_msgs delete_msgs invite_users restrict_members pin_msgs promote_members)) =
		field "chat_id" chat_id <> field "user_id" user_id
		<> field "can_change_info" change_info <> field "can_post_messages" post_msgs
		<> field "can_edit_messages" edit_msgs <> field "can_delete_messages" delete_msgs
		<> field "can_invite_users" invite_users <> field "can_restrict_members" restrict_members
		<> field "can_pin_messages" pin_msgs <> field "can_promote_members" promote_members
	payload (Promote chat_id user_id (Until until_date) (Powers change_info post_msgs edit_msgs delete_msgs invite_users restrict_members pin_msgs promote_members)) =
		field "chat_id" chat_id <> field "user_id" user_id <> field "until_date" until_date
		<> field "can_change_info" change_info <> field "can_post_messages" post_msgs
		<> field "can_edit_messages" edit_msgs <> field "can_delete_messages" delete_msgs
		<> field "can_invite_users" invite_users <> field "can_restrict_members" restrict_members
		<> field "can_pin_messages" pin_msgs <> field "can_promote_members" promote_members
	endpoint _ = "promoteChatMember"
