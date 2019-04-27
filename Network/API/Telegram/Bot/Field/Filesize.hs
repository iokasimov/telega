module Network.API.Telegram.Bot.Field.Filesize (Filesize) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Filesize = Filesize Int deriving Show

instance Accessible Int Filesize where
	access f (Filesize int) = (\int' -> Filesize int') <$> f int

instance FromJSON Filesize where
	parseJSON o = Filesize <$> parseJSON o

instance ToJSON Filesize where
	toJSON (Filesize d) = toJSON d
