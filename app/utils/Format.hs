{-# LANGUAGE OverloadedStrings #-}

module Utils.Format where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (parseQuery)

-- Conversão do tipo ByteString para String ao receber dados do form
parseFormData :: BSL.ByteString -> [(String, String)]
parseFormData bodyLazy = 
    let bodyStrict = BSL.toStrict bodyLazy
        parsed = parseQuery bodyStrict
    in map (\(key, value) -> 
        let keyStr = T.unpack $ TE.decodeUtf8 key
            valStr = maybe "" (T.unpack . TE.decodeUtf8) value
        in (keyStr, valStr)
    ) parsed

timestampToYear :: Int -> Int
timestampToYear timestamp = 
    let secondsInYear = 365 * 24 * 60 * 60
        yearsSince1970 = timestamp `div` secondsInYear
    in 1970 + yearsSince1970