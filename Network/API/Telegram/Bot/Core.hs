module Network.API.Telegram.Bot.Core (Telegram, telegram, environment, Token (..), Ok, result) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Exception (SomeException)
import "base" Data.Bool (Bool (True, False))
import "base" Data.Eq (Eq)
import "base" Data.Either (Either)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Tuple (fst)
import "base" System.IO (IO)
import "joint" Control.Joint (Reader, run, get, (:>))
import "text" Data.Text (Text)

import "base" Debug.Trace (traceShow)

newtype Token = Token Text deriving Eq

type Telegram e = Reader (e, Token) :> Either SomeException :> IO

telegram :: Token -> e -> Telegram e a -> IO (Either SomeException a)
telegram token env = run . flip run (env, token)

environment :: forall e . Telegram e e
environment = fst <$> get @(e, Token)

data Ok a = Ok Bool a

result :: Ok a -> Maybe a
result (Ok True x) = Just x
result (Ok False _ ) = Nothing

instance FromJSON a => FromJSON (Ok a) where
	parseJSON = withObject "Ok" $ \v ->
		Ok <$> v .: "ok" <*> v .: "result"
