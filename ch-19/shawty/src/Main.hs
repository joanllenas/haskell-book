{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

-- test with http://localhost:3000?uri=http://google.com

alphaNum :: String
alphaNum = ['A'] -- fails the second time

-- alphaNum = ['A' .. 'Z'] ++ ['0' .. '9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI ::
  R.Connection ->
  BC.ByteString ->
  BC.ByteString ->
  IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI ::
  R.Connection ->
  BC.ByteString ->
  IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
    [ "<a href=\"",
      shorty,
      "\">Copy and paste your short URL</a>"
    ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat
    [ TL.pack (show resp),
      " shorty is: ",
      TL.pack (linkShorty shawty)
    ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
    [ uri,
      " wasn't a url, did you forget http://?"
    ]

shortyAlreadyExists :: TL.Text
shortyAlreadyExists =
  TL.concat
    [ "The shortcode already exists, try again."
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app ::
  R.Connection ->
  ScottyM ()
app rConn = do
  -- persist
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        eitherShorty <- liftIO (getURI rConn shorty)
        case eitherShorty of
          Left re -> text (TL.pack (show re))
          Right rm -> case rm of
            Just bs -> text "super unlikely"
            Nothing -> do
              let uri' = encodeUtf8 (TL.toStrict uri)
              resp <- liftIO (saveURI rConn shorty uri')
              html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)
  -- retrieve
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where
            tbs :: TL.Text
            tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
