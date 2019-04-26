module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Duration (Duration) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Duration = Duration Int deriving Show

instance Accessible Int Duration where
	access f (Duration int) = (\int' -> Duration int') <$> f int

instance FromJSON Duration where
	parseJSON o = Duration <$> parseJSON o

instance ToJSON Duration where
	toJSON (Duration d) = toJSON d
