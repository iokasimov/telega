module Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Location (Location (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" GHC.Float (Float)
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)

-- import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint)
-- 	, Capacity (Send), Inform (Notify, Silently), Way (Directly, Replying))

data Location = Location Float Float deriving Show

instance FromJSON Location where
	parseJSON = withObject "Location" $ \v -> Location
		<$> v .: "longitude" <*> v .: "latitude"

instance ToJSON Location where
	toJSON (Location latitude longitude) = object
		["latitude" .= latitude, "longitude" .= longitude]

-- instance Persistable ('Send 'Notify 'Directly) Location where
-- 	type instance Payload ('Send 'Notify 'Directly) Location
-- 		= Tagged ('Send 'Notify 'Directly Location) (Int64, Location, Int)
-- 	payload (untag -> (chat_id, Location latitude longitude, live_period)) =
-- 		object ["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude,
-- 			"live_period" .= live_period, "disable_notification" .= False]
-- 	endpoint _ = "sendLocation"
--
-- instance Persistable ('Send 'Silently 'Directly) Location where
-- 	type instance Payload ('Send 'Silently 'Directly) Location
-- 		= Tagged ('Send 'Silently 'Directly Location) (Int64, Location, Int)
-- 	payload (untag -> (chat_id, Location latitude longitude, live_period)) =
-- 		object ["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude,
-- 			"live_period" .= live_period, "disable_notification" .= True]
-- 	endpoint _ = "sendLocation"
--
-- instance Persistable ('Send 'Notify 'Replying) Location where
-- 	type instance Payload ('Send 'Notify 'Replying) Location
-- 		= Tagged ('Send 'Notify 'Replying Location) (Int64, Location, Int, Int)
-- 	payload (untag -> (chat_id, Location latitude longitude, live_period, reply_to_message_id)) = object
-- 		["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude, "live_period" .= live_period,
-- 			"reply_to_message_id" .= reply_to_message_id, "disable_notification" .= False]
-- 	endpoint _ = "sendLocation"
--
-- instance Persistable ('Send 'Silently 'Replying) Location where
-- 	type instance Payload ('Send 'Silently 'Replying) Location
-- 		= Tagged ('Send 'Silently 'Replying Location) (Int64, Location, Int, Int)
-- 	payload (untag -> (chat_id, Location latitude longitude, live_period, reply_to_message_id)) = object
-- 		["chat_id" .= chat_id, "latitude" .= latitude, "longitude" .= longitude, "live_period" .= live_period,
-- 			"reply_to_message_id" .= reply_to_message_id, "disable_notification" .= True]
-- 	endpoint _ = "sendLocation"
