{-# LANGUAGE OverloadedStrings #-}
module Foreign where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Foreign.C

withCStringText :: Text -> (CString -> IO a) -> IO a
withCStringText = BS.useAsCString . Text.encodeUtf8

peekCStringText :: CString -> IO Text
peekCStringText = fmap Text.decodeUtf8 . BS.unsafePackCString
