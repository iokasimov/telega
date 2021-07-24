module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (module Exports, Poll (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)
import "base" Text.Show (Show)

data Poll = Poll Text [Option]
	deriving Show

instance FromJSON Poll where
	parseJSON = withObject "Pool" $ \v ->
		Poll <$> v .: "question" <*> v .: "options"
