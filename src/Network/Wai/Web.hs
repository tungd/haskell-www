{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
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

class ResponseBuilder a where
  buildBody :: a -> Builder
  buildHeaders :: a -> [Header]

  default buildHeaders :: a -> [Header]
  buildHeaders _ = []

instance ResponseBuilder a => ToResponse (a, [Header], Status) where
  respond (builder, headers, status) = pure $ responseBuilder status
    (buildHeaders builder <> headers)
    (buildBody builder)

instance ResponseBuilder a => ToResponse (a, [Header]) where
  respond (builder, headers) = pure $ responseBuilder ok200
    (buildHeaders builder <> headers)
    (buildBody builder)
  -- respond (builder, headers) = respond (builder, headers, ok200)

instance ResponseBuilder a => ToResponse (a, Status) where
  respond (builder, status) = pure $ responseBuilder status
    (buildHeaders builder) (buildBody builder)

instance ResponseBuilder a => ToResponse a where
  respond builder = pure $ responseBuilder ok200
    (buildHeaders builder)
    (buildBody builder)
  
instance ResponseBuilder Builder where
  buildBody = id

instance ResponseBuilder Text where
  buildBody = encodeUtf8Builder
  buildHeaders = const [(hContentType, "text/plain; charset=utf-8")]

instance ResponseBuilder Html where
  buildBody = renderHtmlBuilder
  buildHeaders = const [(hContentType, "text/html; charset=utf-8")]

newtype Xml = Xml { markup :: Markup }

instance ResponseBuilder Xml where
  buildBody = renderHtmlBuilder . markup
  buildHeaders = const [(hContentType, "text/xml; charset=utf-8")]

newtype Json a = Json { json :: a }

instance ToJSON a => ResponseBuilder (Json a) where
  buildBody = fromEncoding . toEncoding . json
  buildHeaders = const [(hContentType, "application/json")]

renderHtml :: Html -> Text
renderHtml = utf8BuilderToText . Utf8Builder . renderHtmlBuilder