# www

Wai helper library. It started out as a colelction of helpers I wrote while
building my own web project in Haskell. As for the package name, when I decided
to extract to its own library I noticed the module name `Network.Wai.Web` would
naturally make for package name `wai-web` and couldn't help myself :)

## Motivation

I wanted a a web library for Haskell, with type-safe routing, monad transformers
yet simple and doesn't requires TemplateHaskell. I looked at the frameworks
available and did not find the one that suits my taste.

- Yesod: TemplateHaskell all over the place
- Scotty, Simple: Text-based routing
- Spock: the name was weird (I'm a Starwars fan, what's up?), no monad
  transformer, and in generall I dislike the fact that everything has to be in
  one place, in one single monad
- Snap: Snap is better than Spock in that it can compose smaller apps - a.k.a
  Snaplets, still no monad transformer
- Fn, Firefly: these are actually very close to my need; in fact they're the
  inspiration for `www`, but IMO they're too far away from the Wai Application
  interface

## Example

I highly recommended using `www` along with `rio` and `data-has`. As for
database my recommendation is `selda`. This example include features required in
a typical webapp:

- Logging
- Database connection
- HTML
- JSON

```haskell
module Main where

import RIO
import Data.Has
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Web
import Web.Routes


data Sitemap = Post Text | Category Text
  deriving (Show, Generic)

instance PathInfo Sitemap


main :: IO ()
main = do
  conn <- sqliteOpen "db.sqlite"
  (logger, closeLogger) <- newLogFunc =<< logOptionsHandle stdout True

  runRIO (logger) server

  seldaClose conn
  closeLogger


server
  :: (Has SeldaConnection env, HasLogFunc env)
  => RIO env ()
server = do
  env <- ask
  liftIO . runEnv 3000
    $ routeT "sitemap.xml" (runRIO env) sitemapHandler
    $ routeT "" (runRIO env) blogHandler
    $ routeT "" (runRIO env) homeHandler
    $ staticApp (defaultWebAppSettings "public")

sitemapHandler
  :: (Has SeldaConnection env, HasLogFunc env)
  => Root -> ApplicationT (RIO env)
sitemapHandler _ _ = respond $ Xml $ ...

blogHandler
  :: (Has SeldaConnection env, HasLogFunc env)
  => Sitemap -> ApplicationT (RIO env)

blogHandler (Blog slug) req = do
  respond $ ...

blogHandler (Category cat) req = do
  respond $ ...

homeHandler
  :: (Has SeldaConnection env, HasLogFunc env)
  => Root -> ApplicationT (RIO env)
homeHandler Root _ = ...
homeHandler (Fallback _) _ = ...
```

## License

Copyright 2018 Tung Dao

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.
