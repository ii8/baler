
module Bin (encode, decode) where

import Data.Word
import Data.Bits
import Data.Monoid
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.ByteString.Builder

import Type

encodeun :: Word64 -> Builder
encodeun v
    | v <= 240 = w8 v
    | v <= 2287 = let v' = v - 240 in w8 ((v' `shiftR` 8) + 241) <> w8 (v' .&. 0xff)
    | v <= 67823 = let v' = v - 2288 in word8 249 <> w8 (v' `shiftR` 8) <> w8 (v' .&. 0xff)
    | otherwise = words 0 v mempty
  where
    words :: Word8 -> Word64 -> Builder -> Builder
    words l 0 b = word8 (247 + l) <> b
    words l v' b = words (l + 1) (v' `shiftR` 8) (w8 (0xff .&. v') <> b)

    w8 = word8 . fromIntegral

decodeun' :: Word8 -> Get Word64
decodeun' a0
    | a0 <= 240 = return $ fromIntegral a0
    | a0 <= 248 = getWord8 >>= (\a1 -> return $ 240 + 256 * (fromIntegral a0 - 241) + fromIntegral a1)
    | a0 == 249 = do
        a1 <- getWord8
        a2 <- getWord8
        return $ 2288 + 256 * (fromIntegral a1) + (fromIntegral a2)
    | otherwise = getBytes (a0 - 247) 0
  where
    getBytes :: Word8 -> Word64 -> Get Word64
    getBytes 0 n = return n
    getBytes l n = getWord8 >>= (\b -> getBytes (l-1) ((n `shiftL` 8) + (fromIntegral b)))

decodeun :: Get Word64
decodeun = getWord8 >>= decodeun'

encode' :: TypeV -> Builder
encode' (U8v w) = word8 w
encode' (U16v w) = word16BE w
encode' (U32v w) = word32BE w
encode' (U64v w) = word64BE w
encode' (I8v i) = int8 i
encode' (I16v i) = int16BE i
encode' (I32v i) = int32BE i
encode' (I64v i) = int64BE i
encode' (F32v f) = floatBE f
encode' (F64v d) = doubleBE d
encode' (UVv w) = encodeun w
encode' (Tuplev x) = foldMap (encode' . snd) x
encode' (Unionv _ n x) = encode' (UVv n)
                      <> encode' x
encode' (Arrayv x) = encode' (UVv $ fromIntegral $ length x)
                  <> foldMap encode' x

encode :: TypeV -> B.ByteString
encode = toLazyByteString . encode'

nth :: [a] -> Word64 -> Maybe a
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs (n - 1)
nth _ _ = Nothing

dec :: (Maybe String, Raw) -> Get (Maybe String, TypeV)
dec (a, b) = (,) a <$> decode' b

decode' :: Raw -> Get TypeV
decode' U8 = U8v <$> getWord8
decode' U16 = U16v <$> getWord16be
decode' U32 = U32v <$> getWord32be
decode' U64 = U64v <$> getWord64be
decode' I8 = I8v <$> getInt8
decode' I16 = I16v <$> getInt16be
decode' I32 = I32v <$> getInt32be
decode' I64 = I64v <$> getInt64be
decode' F32 = F32v <$> getFloatbe
decode' F64 = F64v <$> getDoublebe
decode' UV = UVv <$> decodeun
decode' (NameR _) = undefined
decode' (TupleR t) = Tuplev <$> (mapM dec t)
decode' (UnionR u) = do
    n <- decodeun
    case u `nth` n of
      Nothing -> fail "union index out of bounds"
      Just (annotation, r) -> Unionv annotation n <$> decode' r
decode' (ArrayR a) = Arrayv <$>
    (decodeun >>= return . fromIntegral >>=
    flip replicateM (decode' a))

decode :: Raw -> B.ByteString -> Either String TypeV
decode spec bs =
    case runGetOrFail (decode' spec) bs of
      Right (i, _, t) -> if B.null i
                            then Right t
                            else Left "too much input"
      Left (_, _, e) -> Left e
