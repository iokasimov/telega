module Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Venue (Venue (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)
import Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Location (Location)

data Venue = Venue Title Text Location (Maybe Text) (Maybe Text)

instance FromJSON Venue where
	parseJSON = withObject "Venue" $ \i -> Venue
		<$> i .: "title" <*> i .: "address" <*> i .: "location"
		<*> i .:? "foursquare_id" <*> i .:? "foursquare_type"
