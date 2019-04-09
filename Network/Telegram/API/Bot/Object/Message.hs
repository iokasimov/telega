module Network.Telegram.API.Bot.Object.Message (Message (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withArray, withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative (Applicative ((<*>)), Alternative (empty, (<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Int (Int)
import "base" Text.Show (Show)
import "base" Prelude ((+))
import "text" Data.Text (Text, drop, take)

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Message
	= Textual Int Chat From Text
	| Command Int Chat From Text
	deriving Show

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
		extract_command v (ofs, len) = (take len . drop (ofs + 1)) <$> v .: "text"
