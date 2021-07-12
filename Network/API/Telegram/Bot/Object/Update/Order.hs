module Network.API.Telegram.Bot.Object.Update.Order where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Functor ((<$>))
import "base" Data.Function (($))
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Object.Chat (Chat, ID)
import Network.API.Telegram.Bot.Object.Chat.Group (Group)
import Network.API.Telegram.Bot.Object.Sender (Sender)
import Network.API.Telegram.Bot.Object.Member (Member)
import Network.API.Telegram.Bot.Object.Update.Message.Origin (Origin)
import Network.API.Telegram.Bot.Property (Accessible (access))

data Order = Order (ID Chat) Group Sender Member Member
	deriving Show

instance FromJSON Order where
	parseJSON = withObject "Order" $ \v ->
		Order <$> (v .: "chat" >>= \c -> c .: "id") <*> v .: "chat"
			<*> v .: "from" <*> v .: "old_chat_member" <*> v .: "new_chat_member"

instance Accessible (ID Chat) Order where
	access f (Order chat_id group sender old new) = (\chat_id' -> Order chat_id' group sender old new) <$> f chat_id
