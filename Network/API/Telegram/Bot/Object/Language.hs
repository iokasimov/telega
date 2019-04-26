module Network.API.Telegram.Bot.Object.Language (Language (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

newtype Language = Language Text deriving Show

instance FromJSON Language where
	parseJSON = withText "Language" (pure . Language)

instance ToJSON Language where
	toJSON (Language txt) = String txt
