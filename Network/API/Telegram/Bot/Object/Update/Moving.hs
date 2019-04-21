module Network.API.Telegram.Bot.Object.Update.Moving (module Exports, Moving (..)) where

import Network.API.Telegram.Bot.Object.Update.Moving.Group as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>), (<|>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Sender (Sender)

data Moving
	= Gone Sender Group
	| Joined [Sender] Group
	deriving Show

instance FromJSON Moving where
	parseJSON = withObject "Moving" $ \v ->
		(Gone <$> v .: "left_chat_member" <*> v .: "chat") <|>
		(Joined <$> v .: "new_chat_members" <*> v .: "chat")
