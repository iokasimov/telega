module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo (Photo, Photosize) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Int (Int)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message.Content.File.Special.Filesize (Filesize)
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Photo

data Photosize = Photosize Text Int Int (Maybe Filesize) deriving Show

instance Eq Photosize where
	s == s' = ident s == ident s'

instance Identifiable Photosize where
	type Identificator Photosize = Text
	ident (Photosize file_id _ _ _) = file_id

instance FromJSON Photosize where
	parseJSON = withObject "Photosize" $ \v -> Photosize <$> v .: "file_id"
		<*> v .: "width" <*> v .: "height" <*> v .: "file_size"
