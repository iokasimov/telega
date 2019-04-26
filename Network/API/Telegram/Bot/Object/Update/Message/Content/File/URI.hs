module Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI (URI (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Eq (Eq)
import "base" Data.Function ((.))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

newtype URI = URI Text deriving (Eq, Show)

instance Identifiable URI where
	type Identificator URI = Text
	ident (URI txt) = txt

instance FromJSON URI where
	parseJSON = withText "URI" (pure . URI)

instance ToJSON URI where
	toJSON (URI txt) = String txt
