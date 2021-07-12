module Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Chat (Chat, ID, Channel, Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Origin
	= Private (ID Chat) Sender
	| Group (ID Chat) Group Sender
	| Blog (ID Chat) Channel
	deriving Show

instance Accessible (ID Chat) Origin where
	access f (Private i sender) = (\i' -> Private i' sender) <$> f i
	access f (Group i group sender) = (\i' -> Group i' group sender) <$> f i
	access f (Blog i channel) = (\i' -> Blog i' channel) <$> f i

instance Identifiable Origin where
	type Identificator Origin = ID Chat
	ident (Private i _) = i
	ident (Group i _ _) = i
	ident (Blog i _) = i

instance FromJSON Origin where
	parseJSON = withObject "Message" $ \msg -> msg .: "chat" >>= chat msg where

		chat :: Object -> Value -> Parser Origin
		chat msg = withObject "Chat" $ \ch -> ch .: "type" >>= \case
			("private" :: Text) -> Private <$> ch .: "id" <*> msg .: "from"
			("channel" :: Text) -> Blog <$> ch .: "id" <*> parseJSON (Object ch)
			_ -> Group <$> ch .: "id" <*> parseJSON (Object ch) <*> msg .: "from"
