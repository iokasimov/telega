module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Size (Size (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Int (Int)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Size = Size Text Int Int (Maybe Int) deriving Show

instance FromJSON Size where
	parseJSON = withObject "Size" $ \v -> Size <$> v .: "file_id"
		<*> v .: "width" <*> v .: "height" <*> v .: "file_size"
