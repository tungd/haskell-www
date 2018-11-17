import Network.HTTP.Types
import RIO
import Test.Hspec
import Test.Hspec.Wai
import Web.Routes
import Network.Wai

import Network.Wai.Web


main :: IO ()
main = hspec $ with app $ do
  describe "GET /json-ok" $ do
    it "responds with json" $ do
      get "/json-ok" `shouldRespondWith` "\"Hello, World\""
        { matchHeaders = ["Content-Type" <:> "application/json"] }

  describe "GET /text-ok" $ do
    it "responds with text" $ do
      get "/text-ok" `shouldRespondWith` "Hello, World"
        { matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"] }

  describe "GET /text-status" $ do
    it "responds with text" $ do
      get "/text-status" `shouldRespondWith` 201

  describe "GET /text-headers" $ do
    it "responds with text" $ do
      get "/text-headers" `shouldRespondWith` "Hello, World"
        { matchHeaders = [hAcceptLanguage <:> "en-US"] }

  describe "GET /" $ do
    it "responds with text" $ do
      get "/" `shouldRespondWith` "root"

  describe "GET /fallback" $ do
    it "responds with 200" $ do
      get "/fallback" `shouldRespondWith` "root"

  describe "GET /sitemap.xml" $ do
    it "responds with 200" $ do
      get "/sitemap.xml" `shouldRespondWith` "sitemap"


data Sitemap = JsonOk | TextOk | TextStatus | TextHeaders
  deriving (Show, Generic)

instance PathInfo Sitemap

app :: IO Application
app = pure
  $ routeT "" id handler
  $ routeT "/sitemap.xml" id handleSitemap
  $ routeT "" id handleRoot
  $ rootT id (\_ _ -> respond ("fallback" :: Text))

handler :: Sitemap -> ApplicationT IO 
handler JsonOk _ = respond (Json hello)
handler TextOk _ = respond hello
handler TextStatus _ = respond (hello, created201)
handler TextHeaders _ = respond (hello, [hAcceptLanguage /: "en-US"] :: [Header])

handleRoot :: Root -> ApplicationT IO
handleRoot _ _ = respond ("root" :: Text)

handleSitemap :: Root -> ApplicationT IO
handleSitemap _ _ = respond ("sitemap" :: Text)

hello :: Text
hello = "Hello, World"