{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Redirect (redirect) where

import           Control.Exception
import           Data.String.Conversions
import           Network.HTTP.Types
import           Network.URI
import           Network.Wai
import           Safe

redirect :: String -> Application
redirect destination = case parseURIReference destination of
  Just destURI -> \ request send -> do
    if pathInfo request `elem` [[], [""]]
      then do
        redirectLocation <- mkUrl destURI request
        send $ responseLBS found302 [("Location", cs redirectLocation)] "302 Found"
      else send $ responseLBS notFound404 [] "404 Not Found"
  Nothing -> \ _request _send -> do
    throwIO $ ErrorCall ("redirect: not a valid redirect URI: " ++ destination)

mkUrl :: URI -> Request -> IO String
mkUrl destination request =
  case parseURIReference (cs (rawPathInfo request)) of
    Just currentLocation -> return $ show $
      (destination `relativeTo` normalizeSlash currentLocation)
        `relativeFrom` currentLocation
    Nothing ->
      throwIO $ ErrorCall ("redirect: not a valid rawPathInfo URI: " ++ cs (rawPathInfo request))

normalizeSlash :: URI -> URI
normalizeSlash uri = uri{
  uriPath = case lastMay (uriPath uri) of
    Just '/' -> uriPath uri
    Just _ -> uriPath uri ++ "/"
    Nothing -> "/"
}
