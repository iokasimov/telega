module Network.API.Telegram.Bot.Object.Update.Moving (Moving (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Chat.Group (Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property (Accessible (access))

data Moving
	= Gone Sender Group
	| Joined [Sender] Group
	deriving Show

instance Accessible Group Moving where
	access f (Gone sender group) = (\group' -> Gone sender group') <$> f group
	access f (Joined senders group) = (\group' -> Joined senders group') <$> f group

instance FromJSON Moving where
	parseJSON = withObject "Moving" $ \v ->
		(Gone <$> v .: "left_chat_member" <*> v .: "chat") <|>
		(Joined <$> v .: "new_chat_members" <*> v .: "chat")
