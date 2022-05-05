{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-} 
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Utils
    (   encodeHex
    ,   getHexIntValue
    ,   getHexInt
    ,   getIntsFromToken
    ,   intToBBS
    ,   intsToBS
    ,   getInteger
    ,   strToBS
    ) where

import Data.ByteString qualified as BS                          (ByteString)
import Data.ByteString.Char8 as C8                              (pack)
import PlutusTx.Prelude                                         (abs, appendByteString, BuiltinString, BuiltinByteString,  
                                                                 consByteString, emptyByteString, 
                                                                 indexByteString, Integer, lengthOfByteString, ($), (.), 
                                                                 (==), (>), (<), (+), (-), (++), (<>), Maybe (..), otherwise, 
                                                                 quotRem, remainder, sliceByteString, zero)              
import Prelude qualified as Haskell                             (String)


-- | Get the remainder value of an Integer byte.  This is only
--   a partial conversion, but we are only interested in the
--   the remainder and not the quotient.
{-# INLINEABLE getHexIntValue #-}
getHexIntValue :: Integer -> Integer
getHexIntValue i = i `remainder` 16 -- decode from base16


-- | Create a list of "converted" bytes to Integers from 0-9
{-# INLINEABLE getHexInt #-}
getHexInt :: BuiltinByteString -> [Integer]
getHexInt bbs =
    let len = lengthOfByteString bbs   
    in  if len == 0 then []
        else 
            let byte  = indexByteString bbs 0
                value = getHexIntValue byte 
            in if value > 9 then
                    getHexInt (sliceByteString 1 (len - 1) $ bbs)
                else
                    [value] ++ (getHexInt (sliceByteString 1 (len - 1) $ bbs))


-- | Create a list of Integers from a BuiltinByteString
{-# INLINEABLE bBSToInts #-}
bBSToInts :: BuiltinByteString -> [Integer]
bBSToInts bbs =
    let len = lengthOfByteString bbs   
    in  if len == 0 then []
        else 
            let byte  = indexByteString bbs 0
                value = (byte - 48) -- 48 is ASCII code for '0'
            in [value] ++ (bBSToInts (sliceByteString 1 (len - 1) $ bbs))


-- | Create a BuitinByteString from an Integer
{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS y = consByteString (y + 48) emptyByteString -- 48 is ASCII code for '0'
        

-- | Get the sequence number and the list of Integers from a ticket BuiltinByteString.  
--   The ticket BuiltinByteString always uses the first positive 256 byte only for sequence number
--   and the rest of the digits for the actual winning number
{-# INLINEABLE getIntsFromToken #-}
getIntsFromToken :: BuiltinByteString -> Integer -> (Integer, [Integer])
getIntsFromToken tn diff = 
    let seq  = indexByteString tn 0
        num = sliceByteString 1 diff tn
        in (seq - 48, bBSToInts num) -- 48 is ASCII code for '0'


-- | get an Integer from Maybe Integer otherwise
--   return 0
getInteger :: Maybe Integer -> Integer
getInteger p = case p of
    Nothing -> 0
    Just x -> x


-- | Convert a list of Integers to a BuiltinByteString
intsToBS :: [Integer] -> BuiltinByteString
intsToBS [] = emptyByteString
intsToBS (x:xs) = appendByteString (intToBBS x) (intsToBS xs)


-- | Slow digit-by-digit hex to string converter
intToHexString :: Integer -> BuiltinString
intToHexString i
  | i < zero = "-" <> (go . abs $ i)
  | i == zero = "0"
  | otherwise = go i
  where
    go :: Integer -> BuiltinString
    go arg =
      let (q, r) = arg `quotRem` 16
       in if q == zero
            then hexDigitToString r
            else intToHexString q <> hexDigitToString r

-- | We render bytestrings as their individual code points
encodeHex :: BuiltinByteString -> BuiltinString
encodeHex bbs =
  let len = lengthOfByteString bbs
   in if len == zero
        then ""
        else
          let byte = indexByteString bbs zero
           in "" <> intToHexString byte <> (encodeHex . sliceByteString 1 (len - 1) $ bbs)

-- | This is unsafe, but only ever called internally.
hexDigitToString :: Integer -> BuiltinString
hexDigitToString i
  | i == 0 = "0"
  | i == 1 = "1"
  | i == 2 = "2"
  | i == 3 = "3"
  | i == 4 = "4"
  | i == 5 = "5"
  | i == 6 = "6"
  | i == 7 = "7"
  | i == 8 = "8"
  | i == 9 = "9"
  | i == 10 = "a" 
  | i == 11 = "b" 
  | i == 12 = "c" 
  | i == 13 = "d"
  | i == 14 = "e"
  | otherwise = "f"

-- | String to ByteString converter
strToBS :: Haskell.String -> BS.ByteString
strToBS = C8.pack 