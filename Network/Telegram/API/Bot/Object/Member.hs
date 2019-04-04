module Network.Telegram.API.Bot.Object.Member (Member (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))

import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Member = Gone Chat From | Joined Chat [From] deriving Show

instance FromJSON Member where
	parseJSON = withObject "Member" $ \v -> gone v <|> joined v where

		gone :: Object -> Parser Member
		gone v = Gone <$> v .: "chat" <*> v .: "left_chat_member"

		joined :: Object -> Parser Member
		joined v = Joined <$> v .: "chat" <*> v .: "new_chat_members"
