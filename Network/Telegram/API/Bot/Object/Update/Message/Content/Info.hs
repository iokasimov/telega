module Network.Telegram.API.Bot.Object.Update.Message.Content.Info (Info (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Object, Parser, Value)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.Telegram.API.Bot.Object.Update.Message.Content.Location (Location)

data Info = Point Location
	| Contact (Maybe Int) Text (Maybe Text) Text (Maybe Text)
	| Venue Location Text Text (Maybe Text) (Maybe Text)
	deriving Show

instance FromJSON Info where
	parseJSON = withObject "Info" $ \v -> contact v <|> venue v <|> point v where

		contact :: Object -> Parser Info
		contact v = v .: "contact" >>= info where

			info :: Value -> Parser Info
			info = withObject "Contact" $ \i -> Contact <$> i .:? "user_id"
				<*> i .: "first_name" <*> i .:? "last_name"
				<*> i .: "phone_number" <*> i .:? "vcard"

		venue :: Object -> Parser Info
		venue v = v .: "venue" >>= info where

			info :: Value -> Parser Info
			info = withObject "Venue" $ \i -> Venue
				<$> i .: "location" <*> i .: "title" <*> i .: "address"
				<*> i .:? "foursquare_id" <*> i .:? "foursquare_type"

		point :: Object -> Parser Info
		point v = Point <$> v .: "location"
