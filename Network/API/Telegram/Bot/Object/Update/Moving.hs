module Network.API.Telegram.Bot.Object.Update.Moving (Moving (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (flip, ($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Identifier.Chat (ID)
import Network.API.Telegram.Bot.Object.Chat (Chat)
import Network.API.Telegram.Bot.Object.Chat.Group (Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property (Accessible (access))

data Moving
	= Gone Sender (ID Chat) Group
	| Joined [Sender] (ID Chat) Group
	deriving Show

instance Accessible Group Moving where
	access f (Gone sender i group) = Gone sender i <$> f group
	access f (Joined senders i group) = Joined senders i <$> f group

instance Accessible (ID Chat) Moving where
	access f (Gone sender i group) = flip (Gone sender) group <$> f i
	access f (Joined senders i group) = flip (Joined senders) group <$> f i

instance FromJSON Moving where
	parseJSON = withObject "Moving" $ \msg -> msg .: "chat" >>= chat msg where

		chat :: Object -> Value -> Parser Moving
		chat msg = withObject "Group" $ \g ->
			(Gone <$> msg .: "left_chat_member" <*> g .: "id" <*> parseJSON (Object g)) <|>
			(Joined <$> msg .: "new_chat_members" <*> g .: "id" <*> parseJSON (Object g))
