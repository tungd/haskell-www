{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Wai.Web
  ( ApplicationT
  , RunT
  , Root(..)
  , ToResponse(..)
  , Html
  , Xml(..)
  , Json(..)
  , (/:)
  , routeT
  , renderHtml
  , notFoundText
  ) where

import Data.Aeson (ToJSON(..), fromEncoding)
import Network.HTTP.Types
import Network.Wai
import RIO
import RIO.ByteString (stripPrefix)
import Text.Blaze.Html (Html, Markup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.ParserCombinators.Parsec ()
import Text.ParserCombinators.Parsec.Combinator (notFollowedBy)
import Text.ParserCombinators.Parsec.Prim  ((<?>))
import Web.Routes


type ApplicationT m = Request -> m Response 
type RunT m = m ResponseReceived -> IO ResponseReceived

data Root = Root deriving (Show)
data Fallback = Fallback Text deriving (Show)

instance PathInfo Root where
  toPathSegments Root = []
  fromPathSegments = Root <$ eof

instance PathInfo Fallback where
  toPathSegments (Fallback t) = [t]
  fromPathSegments = Fallback <$> anySegment

eof :: URLParser ()
eof = notFollowedBy anySegment <?> "end of input"


routeT
  :: (PathInfo a, MonadIO m)
  => ByteString -> RunT m -> (a -> ApplicationT m) -> Middleware
routeT root runT app next req resp =
  runT $ case toMaybe . fromPathInfo =<< stripPrefix root url of
    Nothing -> liftIO $ next req resp
    Just path -> liftIO . resp =<< app path req
  where
    url = rawPathInfo req
    toMaybe = either (const Nothing) Just

class ToResponse a where
  respond :: (MonadIO m) => a -> m Response

instance ToResponse b => ToResponse (b, [Header], Status) where
  respond (b, headers, status) = respond b
    <&> mapResponseStatus (const status)
    <&> mapResponseHeaders (<> headers)

instance ToResponse b => ToResponse (b, [Header]) where
  respond (b, headers) = respond (b, headers, ok200)

instance ToResponse b => ToResponse (b, Status) where
  respond (b, status) = respond (b, [] :: [Header], status)

instance ToResponse Builder where
  respond = pure . responseBuilder ok200 [] 

instance ToResponse Text where
  respond = pure
    . responseBuilder ok200 [(hContentType, "text/plain; charset=utf-8")]
    . encodeUtf8Builder

instance ToResponse Html where
  respond = pure
    . responseBuilder ok200 [(hContentType, "text/html; charset=utf-8")]
    . renderHtmlBuilder

newtype Xml = Xml { markup :: Markup }

instance ToResponse Xml where
  respond = pure
    . responseBuilder ok200 [(hContentType, "text/xml; charset=utf-8")]
    . renderHtmlBuilder . markup

newtype Json a = Json { json :: a }

instance ToJSON a => ToResponse (Json a) where
  respond = pure
    . responseBuilder ok200 [(hContentType, "application/json")]
    . fromEncoding . toEncoding . json

-- instance {-# OVERLAPPING #-} ToJSON a => ToResponse a where
--   respond = pure
--     . responseBuilder ok200 [(hContentType, "application/json")]
--     . fromEncoding . toEncoding 

renderHtml :: Html -> Text
renderHtml = utf8BuilderToText . Utf8Builder . renderHtmlBuilder

(/:) :: HeaderName -> ByteString -> Header
(/:) = (,)

notFoundText :: Application
notFoundText _ resp = resp =<< respond ("Not found" :: Text)