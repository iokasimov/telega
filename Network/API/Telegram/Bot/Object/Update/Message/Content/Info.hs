module Network.API.Telegram.Bot.Object.Update.Message.Content.Info (Info (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Location as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Info = Point Location
	| Contact Text (Maybe Text) Text (Maybe Text)
	| Venue Location Text Text (Maybe Text) (Maybe Text)
	deriving Show

instance FromJSON Info where
	parseJSON = withObject "Info" $ \v -> contact v <|> venue v <|> point v where

		contact :: Object -> Parser Info
		contact v = v .: "contact" >>= info where

			info :: Value -> Parser Info
			info = withObject "Contact" $ \i -> Contact
				<$> i .: "first_name" <*> i .:? "last_name"
				<*> i .: "phone_number" <*> i .:? "vcard"

		venue :: Object -> Parser Info
		venue v = v .: "venue" >>= info where

			info :: Value -> Parser Info
			info = withObject "Venue" $ \i -> Venue
				<$> i .: "location" <*> i .: "title" <*> i .: "address"
				<*> i .:? "foursquare_id" <*> i .:? "foursquare_type"

		point :: Object -> Parser Info
		point v = Point <$> v .: "location"
