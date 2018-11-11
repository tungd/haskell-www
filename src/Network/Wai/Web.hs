module Network.Wai.Web where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai
import RIO
import RIO.ByteString (stripPrefix)
import Text.Blaze.Html (Html, Markup)
import Text.Blaze.Html.Renderer.Utf8
import Web.Routes

type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type RunT m = m ResponseReceived -> IO ResponseReceived

routeT
  :: (PathInfo a, MonadIO m)
  => ByteString -> RunT m -> (a -> ApplicationT m) -> Middleware
routeT root runT app next req resp =
  runT $ case toMaybe . fromPathInfo =<< stripPrefix root url of
    Nothing -> liftIO $ next req resp
    Just path -> app path req (liftIO . resp)
  where
    url = rawPathInfo req
    toMaybe = either (const Nothing) Just

rootT
  :: (MonadIO m)
  => RunT m -> ([Text] -> ApplicationT m) -> Application
rootT runT app req resp = runT (app url req (liftIO . resp))
  where
    url = decodePathInfo (rawPathInfo req)

okText :: Builder -> Response
okText = errText ok200

errText :: Status -> Builder -> Response
errText status = responseBuilder status []

okHtml :: Html -> Response
okHtml = errHtml ok200

errHtml :: Status -> Html -> Response
errHtml status = responseBuilder status
  [ (hContentType, "text/html; charset=utf-8") ] . renderHtmlBuilder

okXml :: Markup -> Response
okXml = errXml ok200

errXml :: Status -> Markup -> Response
errXml status = responseBuilder status
  [ (hContentType, "text/xml; charset=utf-8") ] . renderHtmlBuilder

okJson :: ToJSON a => a -> Response
okJson = errJson ok200

errJson :: ToJSON a => Status -> a -> Response
errJson status = responseBuilder status
  [ (hContentType, "application/json; charset=utf-8") ] . fromEncoding . toEncoding

displayBuilder :: (Display a) => a -> Builder
displayBuilder = getUtf8Builder . display

renderHtml :: Html -> Text
renderHtml = utf8BuilderToText . Utf8Builder . renderHtmlBuilder