module Network.API.Telegram.Bot.Object.Update.Message.Content.File.URI (URI (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withObject, (.:))
import "base" Data.Functor ((<$>))
import "base" Data.Function (($))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

newtype URI = URI Text
	deriving Show

instance FromJSON URI where
	parseJSON = withObject "URI" $ \v ->
		URI <$> v .: "file_id"

instance ToJSON URI where
	toJSON (URI txt) = String txt
