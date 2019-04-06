module Network.Telegram.API.Bot.Object.Moving (Moving (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative (Alternative ((<|>)))

import Network.Telegram.API.Bot.Access (Access (access))
import Network.Telegram.API.Bot.Object.Chat (Chat)
import Network.Telegram.API.Bot.Object.From (From)

data Moving = Gone Chat From | Joined Chat [From] deriving Show

instance FromJSON Moving where
	parseJSON = withObject "Moving" $ \v -> gone v <|> joined v where

		gone :: Object -> Parser Moving
		gone v = Gone <$> v .: "chat" <*> v .: "left_chat_member"

		joined :: Object -> Parser Moving
		joined v = Joined <$> v .: "chat" <*> v .: "new_chat_members"

instance Access Chat Moving where
	access f (Gone chat from) = (\chat' -> Gone chat' from) <$> f chat
	access f (Joined chat froms) = (\chat' -> Joined chat' froms) <$> f chat
