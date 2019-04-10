module Network.Telegram.API.Bot.Object.Content (Content (..)) where

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

data Content
	= Textual Text
	| Command Text
	deriving Show

instance FromJSON Content where
	parseJSON = withObject "Content" $ \v -> command v <|> textual v where

		command :: Object -> Parser Content
		command v = Command <$> (v .: "entities" >>= command_entity >>= extract_command v)

		textual :: Object -> Parser Content
		textual v = Textual <$> v .: "text"

		command_entity :: Value -> Parser (Int, Int)
		command_entity = withArray "Command content" $ \a ->
			foldr ((<|>) . entity) empty a where

			entity :: Value -> Parser (Int, Int)
			entity = withObject "Command entity" $ \v -> v .: "type" >>= \case
				("bot_command" :: Text) -> (,) <$> v .: "offset" <*> v .: "length"
				_ -> fail "It's not a bot command"

		extract_command :: Object -> (Int, Int) -> Parser Text
		extract_command v (ofs, len) = (take len . drop (ofs + 1)) <$> v .: "text"
