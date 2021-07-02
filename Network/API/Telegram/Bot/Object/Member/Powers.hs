module Network.API.Telegram.Bot.Object.Member.Powers (Powers (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Data.Bool (Bool)
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

data Powers = Powers Bool Bool Bool Bool Bool Bool Bool Bool
	deriving Show

instance FromJSON Powers where
	parseJSON = withObject "Powers" $ \v -> Powers
		<$> v .: "can_change_info"
		<*> v .: "can_post_messages"
		<*> v .: "can_edit_messages"
		<*> v .: "can_delete_messages"
		<*> v .: "can_invite_users"
		<*> v .: "can_restrict_members"
		<*> v .: "can_pin_messages"
		<*> v .: "can_promote_members"
