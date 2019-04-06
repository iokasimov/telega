module Network.Telegram.API.Bot.Object.Message
	(Message (..), Initial, Marking) where

import "aeson" Data.Aeson (FromJSON (parseJSON), object, withArray, withObject, (.:), (.=))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative (Alternative ((<|>), empty))
import "base" Data.Int (Int64)
import "text" Data.Text (Text)

import qualified "text" Data.Text as T (drop, take)

import Network.Telegram.API.Bot.Access (Access (access))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)
import Network.Telegram.API.Bot.Object.Keyboard (Keyboard)
import Network.Telegram.API.Bot.Capacity.Postable
	(Initial, Postable (initial_value, post_endpoint))
import Network.Telegram.API.Bot.Capacity.Purgeable
	(Marking, Purgeable (marking_value, purge_endpoint))
import Network.Telegram.API.Bot.Property.Identifiable
	(Identifiable (identificator), Identificator)

data Message
	= Textual Int Chat From Text
	| Command Int Chat From Text
	deriving Show

type instance Initial Message = (Int64, Text, Maybe Keyboard)
type instance Marking Message = (Int64, Int)
type instance Identificator Message = Int

instance Identifiable Message where
	identificator (Textual i _ _ _) = i
	identificator (Command i _ _ _) = i

instance FromJSON Message where
	parseJSON = withObject "Message" $ \v -> command v <|> textual v where

		command :: Object -> Parser Message
		command v = Command <$> v .: "message_id" <*> v .: "chat" <*> v .: "from"
			<*> (v .: "entities" >>= command_entity >>= extract_command v)

		textual :: Object -> Parser Message
		textual v = Textual <$> v .: "message_id"
			<*> v .: "chat" <*> v .: "from" <*> v .: "text"

		command_entity :: Value -> Parser (Int, Int)
		command_entity = withArray "Command entity" $ \a ->
			foldr ((<|>) . entity) empty a where

			entity :: Value -> Parser (Int, Int)
			entity = withObject "Command entity" $ \v -> v .: "type" >>= \case
				("bot_command" :: Text) -> (,) <$> v .: "offset" <*> v .: "length"
				_ -> fail "It's not a bot command"

		extract_command :: Object -> (Int, Int) -> Parser Text
		extract_command v (ofs, len) = (T.take len . T.drop (ofs + 1)) <$> v .: "text"

instance Postable Message where
	initial_value (chat_id, text, Nothing) =
		object ["chat_id" .= chat_id, "text" .= text]
	initial_value (chat_id, text, Just kb) =
		object ["chat_id" .= chat_id, "text" .= text, "reply_markup" .= kb]
	post_endpoint _ = "sendMessage"

instance Purgeable Message where
	marking_value (chat_id, message_id) =
		object ["chat_id" .= chat_id, "message_id" .= message_id]
	purge_endpoint _ = "deleteMessage"

instance Access Chat Message where
	access f (Textual msg_id chat from txt) = (\chat' -> Textual msg_id chat' from txt) <$> f chat
	access f (Command msg_id chat from cmd) = (\chat' -> Command msg_id chat' from cmd) <$> f chat

instance Access From Message where
	access f (Textual msg_id chat from txt) = (\from' -> Textual msg_id chat from' txt) <$> f from
	access f (Command msg_id chat from cmd) = (\from' -> Command msg_id chat from' cmd) <$> f from
