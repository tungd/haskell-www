import RIO
import Test.Hspec
import Test.Hspec.Wai
import Web.Routes
import System.IO

import Network.Wai.Web


main :: IO ()
main = hspec $ with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` "Hello, World"
        { matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"] }

  describe "GET /json-route" $ do
    it "responds with json" $ do
      get "/json-route" `shouldRespondWith` "\"Hello, World\""
        { matchHeaders = ["Content-Type" <:> "application/json"] }

  describe "GET /text-route" $ do
    it "responds with text" $ do
      get "/text-route" `shouldRespondWith` "Hello, World"
        { matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"] }


data Sitemap = JsonRoute | TextRoute | HtmlRoute
  deriving (Show, Generic)

instance PathInfo Sitemap

app = pure
  $ routeT "" id handler
  $ rootT id (\_ _ -> respond hello)
  where
    toMaybe = either (const Nothing) Just

handler :: Sitemap -> ApplicationT IO 
handler JsonRoute _ = respond (Json hello)
handler TextRoute _ = respond hello

hello :: Text
hello = "Hello, World"