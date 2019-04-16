module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (Poll (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Bool (bool)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Poll
	= Opened Text Text [Option]
	| Closed Text Text [Option]
	deriving Show

type State = Text -> Text -> [Option] -> Poll

instance FromJSON Poll where
	parseJSON = withObject "Pool" $ \v -> v .: "is_closed"
		>>= bool (state v Opened) (state v Closed) where

		state :: Object -> State -> Parser Poll
		state v f = f <$> v .: "id" <*> v .: "question" <*> v .: "options"
