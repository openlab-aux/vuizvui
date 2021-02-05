{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Emoji.DataFiles.EmojiTest

import Codec.Binary.UTF8.Light (w2c)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import Data.Text (Text ())
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word32)

parseEmojiTest :: Text -> Either String EmojiTest
parseEmojiTest = A.parseOnly emojiTestFile

reportError :: Either String a -> IO a
reportError (Left e)  = fail e
reportError (Right x) = pure x

outputTepFormat :: EmojiTest -> IO ()
outputTepFormat = mapM_ (outputEntry [])
  where outputEntry p (Comment _) = pure ()
        outputEntry p (Group _ name es) =
          mapM_ (outputEntry (p ++ [name])) es
        outputEntry p (Entry codes _ _ name) =
          outputEmoji codes name p

codesText :: [Word32] -> Text
codesText = T.pack . map w2c

outputEmoji :: [Word32] -> Text -> [Text] -> IO ()
outputEmoji codes name path = B.putStr . T.encodeUtf8
  $  codesText codes
  <> " " <> name <> " ("
  <> T.intercalate " → " path <> ")\n"

readStdin :: IO Text
readStdin = T.decodeUtf8 <$> B.getContents

main :: IO ()
main = readStdin >>= reportError . parseEmojiTest >>= outputTepFormat
