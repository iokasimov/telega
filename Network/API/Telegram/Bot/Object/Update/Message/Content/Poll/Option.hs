module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option (Option (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Option = Option Text Int deriving Show

instance FromJSON Option where
	parseJSON = withObject "Option" $ \v -> Option
		<$> v .: "text" <*> v .: "voter_counter"
