module Network.API.Telegram.Bot.Field.Language (Language (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Text.Show (Show)
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Language = Language Text
	deriving Show

instance Accessible Text Language where
	access f (Language txt) = (\txt' -> Language txt') <$> f txt

instance FromJSON Language where
	parseJSON = withText "Language" (pure . Language)

instance ToJSON Language where
	toJSON (Language txt) = String txt
