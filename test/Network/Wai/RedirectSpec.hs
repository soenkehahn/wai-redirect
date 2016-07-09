{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.RedirectSpec where

import           Blaze.ByteString.Builder
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.String.Conversions
import           Data.Text (Text)
import           GHC.Stack
import           Network.HTTP.Client as Client
import           Network.HTTP.Types
import           Network.Wai hiding (responseHeaders, responseStatus, Response)
import           Network.Wai.Handler.Warp
import           Test.Hspec

import           Network.Wai.Redirect

isJust :: Maybe a -> Bool
isJust = \ case
  Nothing -> False
  Just _ -> True

spec :: Spec
spec = do
  describe "redirect" $ do
    it "returns a 302" $
      withApp (redirect "/foo") $ \ port -> do
        response <- requestRaw port [""]
        responseStatus response `shouldBe` found302

    it "server a Location header" $
      withApp (redirect "/foo") $ \ port -> do
        response <- requestRaw port [""]
        lookup "Location" (responseHeaders response) `shouldSatisfy` isJust

    it "throws 404s for trailing path segments" $ do
      withApp (redirect "http://example.com") $ \ port -> do
        response <- requestRaw port ["foo"]
        responseStatus response `shouldBe` notFound404

    testRedirectSpec (Redirect "http://example.com/foo") [] "http://example.com/foo"
    testRedirectSpec (Redirect "http://example.com/foo") [""] "http://example.com/foo"

    testRedirectSpec ("foo" :/ Redirect "new") ["foo"] "/foo/new"
    testRedirectSpec ("foo" :/ Redirect "new") ["foo", ""] "new"

    testRedirectSpec ("foo" :/ Redirect "./new") ["foo"] "/foo/new"
    testRedirectSpec ("foo" :/ Redirect "./new") ["foo", ""] "new"

    testRedirectSpec ("foo" :/ "bar" :/ Redirect "./new") ["foo", "bar"] "bar/new"
    testRedirectSpec ("foo" :/ "bar" :/ Redirect "./new") ["foo", "bar", ""] "new"

    testRedirectSpec ("foo" :/ "bar" :/ Redirect "../new") ["foo", "bar"] "new"
    testRedirectSpec ("foo" :/ "bar" :/ Redirect "../new") ["foo", "bar", ""] "../new"
    -- fixme: test absolute paths
    -- fixme: redirect /foo/boo -> /foo/new/boo

testRedirectSpec :: (?loc :: CallStack) =>
  App -> [Text] -> ByteString -> Spec
testRedirectSpec app currentLocation expected = do
  it ("App: " ++ show app ++ " | Request: " ++ show currentLocation ++ " --> " ++ show expected) $ do
    testRedirect app currentLocation expected

testRedirect :: (?loc :: CallStack) => App -> [Text] -> ByteString -> IO ()
testRedirect application currentLocation expected =
  withApp (toApplication application) $ \ port -> do
    response <- requestRaw port currentLocation
    assert (responseStatus response == found302) (return ())
    case lookup "Location" (responseHeaders response) of
      Just result -> when (result /= expected) $
        result `shouldBe` expected
      Nothing -> throwIO $ ErrorCall $
        "no Location header"

requestRaw :: Int -> [Text] -> IO (Response ())
requestRaw port pathSegments = do
  let path = cs $ toLazyByteString $ encodePathSegments pathSegments
  manager <- newManager defaultManagerSettings
  request <- noRedirection <$>
    parseUrlThrow ("http://localhost:" ++ show port ++ path)
  httpNoBody request manager

noRedirection :: Client.Request -> Client.Request
noRedirection r = r{
  redirectCount = 0,
  checkResponse = \ _ _ -> return ()
}

withApp :: Application -> (Int -> IO ()) -> IO ()
withApp app = testWithApplication (return app)

data App
  = Text :/ App
  | Redirect String
  deriving (Show)
infixr :/

toApplication :: App -> Application
toApplication = \ case
  Redirect p -> redirect p
  path :/ app -> \ request send -> do
    case pathInfo request of
      rPath : rest | rPath == path ->
        toApplication app (request{ pathInfo = rest }) send
      _ -> send $ responseLBS notFound404 [] "not found"

(/-) :: Text -> Application -> Application
path /- app = \ request send ->
  case pathInfo request of
    reqPath : rest -> if reqPath == path
      then app (request{
        pathInfo = rest
      }) send
      else send notFound
    [] -> send notFound
  where
    notFound = responseLBS notFound404 [] ""
infixr /-

foo :: Application
foo = "foo" /- redirect "bar"
