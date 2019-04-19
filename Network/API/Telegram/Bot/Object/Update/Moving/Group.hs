module Network.API.Telegram.Bot.Object.Update.Moving.Group (Group (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

data Group
	= Basic Int64 Text
	| Super Int64 Text (Maybe Text)
	deriving Show

instance FromJSON Group where
	parseJSON = withObject "Group" $ \v -> v .: "type" >>= \case
		("group" :: Text) -> Basic <$> v .: "id" <*> v .: "title"
		("supergroup" :: Text) -> Super <$> v .: "id" <*> v .: "title" <*> v .:? "description"
		_ -> fail "Neither group nor supergroup!"
