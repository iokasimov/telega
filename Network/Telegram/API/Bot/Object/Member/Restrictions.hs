module Network.Telegram.API.Bot.Object.Member.Restrictions (Restrictions (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Data.Bool (Bool)
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

data Restrictions = Restrictions Bool Bool Bool Bool deriving Show

instance FromJSON Restrictions where
	parseJSON = withObject "Restrictions" $ \v -> Restrictions
		<$> v .: "can_send_messages"
		<*> v .: "can_send_media_messages"
		<*> v .: "can_send_other_messages"
		<*> v .: "can_add_web_page_previews"
