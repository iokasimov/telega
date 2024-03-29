module Network.API.Telegram.Bot.Field.Title where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Title = Title Text
	deriving Show

instance Accessible Text Title where
	access f (Title txt) = (\txt' -> Title txt') <$> f txt

instance FromJSON Title where
	parseJSON o = Title <$> parseJSON o

instance ToJSON Title where
	toJSON (Title d) = toJSON d
