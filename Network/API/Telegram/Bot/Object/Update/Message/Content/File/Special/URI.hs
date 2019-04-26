module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.URI (URI (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import "base" Control.Applicative (pure)
import "base" Data.Eq (Eq)
import "base" Data.Function ((.))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property (Identifiable (Identificator, ident))

newtype URI = URI Text deriving (Eq, Show)

instance Accessible Text URI where
	access f (URI txt) = (\txt' -> URI txt') <$> f txt

instance Identifiable URI where
	type Identificator URI = Text
	ident (URI txt) = txt

instance FromJSON URI where
	parseJSON = withText "URI" (pure . URI)

instance ToJSON URI where
	toJSON (URI txt) = toJSON txt
