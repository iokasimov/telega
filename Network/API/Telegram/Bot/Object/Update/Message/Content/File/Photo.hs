module Network.API.Telegram.Bot.Object.Update.Message.Content.File.Photo (Photo, Photosize) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)

import Network.API.Telegram.Bot.Field (Height, Width, Filesize, URI)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident))

data Photo

data Photosize = Photosize URI Height Width (Maybe Filesize)

instance Eq Photosize where
	s == s' = ident s == ident s'

instance Accessible Height Photosize where
	access f (Photosize uri height width fs) =
		(\height' -> Photosize uri height' width fs) <$> f height

instance Accessible Width Photosize where
	access f (Photosize uri height width fs) =
		(\width' -> Photosize uri height width' fs) <$> f width

instance Identifiable Photosize where
	type Identificator Photosize = URI
	ident (Photosize uri _ _ _) = uri

instance FromJSON Photosize where
	parseJSON = withObject "Photosize" $ \v -> Photosize <$> v .: "file_id"
		<*> v .: "width" <*> v .: "height" <*> v .: "file_size"
