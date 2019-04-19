module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Voice (Voice (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Voice = Voice Int (Maybe Text) (Maybe Int) deriving Show

instance FromJSON Voice where
	parseJSON = withObject "Voice" $ \v -> Voice <$>
		v .: "duration" <*> v .:? "mime_type" <*> v .:? "file_size"
