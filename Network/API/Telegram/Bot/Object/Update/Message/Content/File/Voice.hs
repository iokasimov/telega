module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice (Voice (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, object, withObject, (.:), (.:?), (.=))
import "base" Control.Applicative ((<*>))
import "base" Data.Bool (Bool (True, False))
import "base" Data.Int (Int, Int64)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

data Voice = Voice Text Int (Maybe Text) (Maybe Int) deriving Show

instance FromJSON Voice where
	parseJSON = withObject "Voice" $ \v -> Voice <$> v .: "file_id"
		<*> v .: "duration" <*> v .:? "mime_type" <*> v .:? "file_size"
