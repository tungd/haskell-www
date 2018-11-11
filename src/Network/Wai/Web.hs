{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Wai.Web where

import Data.Aeson (ToJSON(..), fromEncoding)
import Network.HTTP.Types
import Network.Wai
import RIO
import RIO.ByteString (stripPrefix)
import Text.Blaze.Html (Html, Markup)
import Text.Blaze.Html.Renderer.Utf8
import Web.Routes

type ApplicationT m = Request -> m Response 
type RunT m = m ResponseReceived -> IO ResponseReceived

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

rootT
  :: (MonadIO m)
  => RunT m -> ([Text] -> ApplicationT m) -> Application
rootT runT app req resp = runT (liftIO . resp =<< app url req)
  where
    url = decodePathInfo (rawPathInfo req)

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