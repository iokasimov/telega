module Network.API.Telegram.Bot.Object.Update.Message.Content.Location (Location (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" GHC.Float (Float)
import "base" Text.Show (Show)

data Location = Location Float Float
	deriving Show

instance FromJSON Location where
	parseJSON = withObject "Location" $ \v -> Location
		<$> v .: "longitude" <*> v .: "latitude"

instance ToJSON Location where
	toJSON (Location latitude longitude) = object
		["latitude" .= latitude, "longitude" .= longitude]
