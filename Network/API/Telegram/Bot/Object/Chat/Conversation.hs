module Network.API.Telegram.Bot.Object.Chat.Conversation (Conversation (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Conversation = Conversation Int64 deriving Show

instance Identifiable Conversation where
	type Identificator Conversation = Int64
	ident (Conversation i) = i

instance FromJSON Conversation where
	parseJSON = withObject "Conversation" $ \v -> v .: "type" >>= \case
		("private" :: Text) -> Conversation <$> v .: "id"
		_ -> fail "Not a private chat!"
