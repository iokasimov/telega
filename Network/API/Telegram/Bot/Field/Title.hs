module Network.API.Telegram.Bot.Field.Title (Title) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Title = Title Int deriving Show

instance Accessible Int Title where
	access f (Title int) = (\int' -> Title int') <$> f int

instance FromJSON Title where
	parseJSON o = Title <$> parseJSON o

instance ToJSON Title where
	toJSON (Title d) = toJSON d
