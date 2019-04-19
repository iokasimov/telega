module Network.API.Telegram.Bot.Object.Update.Message.Content (Content (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.File as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.Info as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withArray, withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Object, Parser, Value (Object))
import "base" Control.Applicative (Applicative ((<*>)), Alternative (empty, (<|>)))
import "base" Control.Monad (Monad ((>>=)), fail)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "base" Prelude ((+))
import "text" Data.Text (Text, drop, take)

data Content
	= Textual Text
	| Command Text
	| Attachment (Maybe Caption) File
	| Polling Text Poll
	| Information Info
	deriving Show

instance FromJSON Content where
	parseJSON = withObject "Content" $ \v -> command v <|> attachment v <|> information v <|> polling v <|> textual v where

		command :: Object -> Parser Content
		command v = Command <$> (v .: "entities" >>= command_entity >>= extract_command v)

		command_entity :: Value -> Parser (Int, Int)
		command_entity = withArray "Command content" $ \a ->
			foldr ((<|>) . entity) empty a where

			entity :: Value -> Parser (Int, Int)
			entity = withObject "Command entity" $ \v -> v .: "type" >>= \case
				("bot_command" :: Text) -> (,) <$> v .: "offset" <*> v .: "length"
				_ -> fail "It's not a bot command"

		extract_command :: Object -> (Int, Int) -> Parser Text
		extract_command v (ofs, len) = (take len . drop (ofs + 1)) <$> v .: "text"

		attachment :: Object -> Parser Content
		attachment v = Attachment <$> v .:? "caption" <*> parseJSON (Object v)

		information :: Object -> Parser Content
		information v = Information <$> parseJSON (Object v)

		polling :: Object -> Parser Content
		polling v = Polling <$> (v .: "poll" >>= poll_id) <*> v .: "poll" where

			poll_id :: Value -> Parser Text
			poll_id = withObject "Poll" $ \p -> p .: "id"

		textual :: Object -> Parser Content
		textual v = Textual <$> v .: "text"
