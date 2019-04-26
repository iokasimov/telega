module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Caption (Caption (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Caption = Caption Text deriving Show

instance Accessible Text Caption where
	access f (Caption txt) = (\txt' -> Caption txt') <$> f txt

instance FromJSON Caption where
	parseJSON = withText "Caption" (pure . Caption)

instance ToJSON Caption where
	toJSON (Caption txt) = toJSON txt
