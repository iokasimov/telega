module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (Poll (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Poll = Poll Text [Option]
	deriving Show

instance FromJSON Poll where
	parseJSON = withObject "Pool" $ \v ->
		Poll <$> v .: "question" <*> v .: "options"
