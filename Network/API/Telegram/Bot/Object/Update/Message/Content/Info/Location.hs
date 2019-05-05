module Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Location (Location (..), Live (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" GHC.Float (Float)

data Location = Location Float Float

instance FromJSON Location where
	parseJSON = withObject "Location" $ \v -> Location
		<$> v .: "longitude" <*> v .: "latitude"

instance ToJSON Location where
	toJSON (Location latitude longitude) = object
		["latitude" .= latitude, "longitude" .= longitude]

data Live a where Live :: Int -> Location -> Live Location
