module Network.API.Telegram.Bot.Object.Chat.Group (Group (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "base" Data.Maybe (Maybe)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)

data Group = Basic Title | Super Title (Maybe Text)
	deriving Show

instance FromJSON Group where
	parseJSON = withObject "Group" $ \chat -> chat .: "type" >>= \case
		("supergroup" :: Text) -> Super <$> chat .: "title" <*> chat .:? "description"
		("group" :: Text) -> Basic <$> chat .: "title"
		_ -> fail "Neither group nor supergroup!"
