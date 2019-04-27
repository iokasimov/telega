module Network.API.Telegram.Bot.Object.Update.Moving.Group (Group (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Group
	= Basic Int64 Title
	| Super Int64 Title (Maybe Text)
	deriving Show

instance Identifiable Group where
	type Identificator Group = Int64
	ident (Basic i _ ) = i
	ident (Super i _ _) = i

instance FromJSON Group where
	parseJSON = withObject "Group" $ \v -> v .: "type" >>= \case
		("group" :: Text) -> Basic <$> v .: "id" <*> v .: "title"
		("supergroup" :: Text) -> Super <$> v .: "id" <*> v .: "title" <*> v .:? "description"
		_ -> fail "Neither group nor supergroup!"
