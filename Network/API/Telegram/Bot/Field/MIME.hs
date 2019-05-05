module Network.API.Telegram.Bot.Field.MIME (MIME (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype MIME = MIME Text

instance Accessible Text MIME where
	access f (MIME txt) = (\txt' -> MIME txt') <$> f txt

instance FromJSON MIME where
	parseJSON = withText "MIME" (pure . MIME)

instance ToJSON MIME where
	toJSON (MIME txt) = toJSON txt
