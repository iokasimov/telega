module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Caption (Caption (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

newtype Caption = Caption Text deriving Show

instance FromJSON Caption where
	parseJSON = withText "Caption" (pure . Caption)

instance ToJSON Caption where
	toJSON (Caption txt) = String txt
