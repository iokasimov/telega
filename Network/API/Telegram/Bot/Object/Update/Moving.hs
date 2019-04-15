module Network.API.Telegram.Bot.Object.Update.Moving (Moving (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative ((<*>), (<|>))
import "base" Control.Monad ((>>=))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Sender (Sender)

data Moving
	= Gone Sender (Int64, Text)
	| Joined [Sender] (Int64, Text)
	deriving Show

instance FromJSON Moving where
	parseJSON = withObject "Moving" $ \v -> gone v <|> joined v where

		gone :: Object -> Parser Moving
		gone v = Gone <$> v .: "left_chat_member" <*> (v .: "chat" >>= chat)

		joined :: Object -> Parser Moving
		joined v = Joined <$> v .: "new_chat_members" <*> (v .: "chat" >>= chat)

		chat :: Object -> Parser (Int64, Text)
		chat c = (,) <$> c .: "id" <*> c .: "title"
