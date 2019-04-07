module Network.Telegram.API.Bot.Core (Telegram, telegram, ask', Token (..), Ok, result) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Exception (SomeException)
import "base" Data.Bool (Bool (True, False))
import "base" Data.Eq (Eq)
import "base" Data.Either (Either)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Tuple (fst)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import "transformers" Control.Monad.Trans.Except (ExceptT, runExceptT)
import "wreq" Network.Wreq.Session (Session)

newtype Token = Token Text deriving Eq

type Telegram e a = ReaderT (e, (Session, Token)) (ExceptT SomeException IO) a

telegram :: Session -> Token -> e -> Telegram e a -> IO (Either SomeException a)
telegram session token env = runExceptT . flip runReaderT (env, (session, token))

ask' :: Telegram e e
ask' = fst <$> ask

data Ok a = Ok Bool a deriving Show

result :: Ok a -> Maybe a
result (Ok True x) = Just x
result (Ok False _ ) = Nothing

instance FromJSON a => FromJSON (Ok a) where
	parseJSON = withObject "Ok" $ \v ->
		Ok <$> v .: "ok" <*> v .: "result"
