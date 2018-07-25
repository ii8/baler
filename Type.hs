
module Type where

import Data.Word
import Data.Bits ((.|.), (.&.), shiftL)
import Data.Char (chr, isPrint)

import Data.Int

data Raw
  = U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  | F32 | F64
  | UV
  | NameR String
  | TupleR [(Maybe String, Raw)]
  | UnionR [(Maybe String, Raw)]
  | ArrayR Raw

ss :: String -> ShowS
ss = showString

showIndent :: Int -> ShowS
showIndent i = ss (replicate (i * 2) ' ')

showAnnotated :: Int -> (Int -> a -> ShowS) -> (Maybe String, a) -> ShowS
showAnnotated i f (Nothing, r) = f i r
showAnnotated i f (Just a, r) = ss a . ss ": " . f i r

showItem :: Int -> (Int -> a -> ShowS) -> (Maybe String, a) -> ShowS
showItem i f p = showIndent i . showAnnotated i f p

showBlock :: Int -> (Int -> a -> ShowS)
          -> String -> [(Maybe String, a)] -> String
          -> ShowS
showBlock _ _ start [] end = ss start . showChar ' ' . ss end
showBlock i f start l end =
    let i' = i + 1 in
    ss start . showChar '\n'
    . foldl (\s p -> s . showItem i' f p . showChar '\n') (ss "") l
    . showIndent i . ss end

showType :: Int -> Raw -> ShowS
showType _ U8 = ss "u8"
showType _ U16 = ss "u16"
showType _ U32 = ss "u32"
showType _ U64 = ss "u64"
showType _ I8 = ss "i8"
showType _ I16 = ss "i16"
showType _ I32 = ss "i32"
showType _ I64 = ss "i64"
showType _ F32 = ss "f32"
showType _ F64 = ss "f64"
showType _ UV = ss "uv"
showType _ (NameR s) = ss s
showType i (TupleR l) = showBlock i showType "tuple" l "end"
showType i (UnionR l) = showBlock i showType "union" l "end"
showType i (ArrayR t) = ss "array\n" . showIndent (i + 1) . showType (i + 1) t

instance Show Raw where
  showsPrec _ a = showType 0 a

data TypeV
  = U8v Word8 | U16v Word16 | U32v Word32 | U64v Word64
  | I8v Int8 | I16v Int16 | I32v Int32 | I64v Int64
  | F32v Float | F64v Double
  | UVv Word64
  | Tuplev [(Maybe String, TypeV)]
  | Unionv (Maybe String) Word64 TypeV
  | Arrayv [TypeV]

sp :: (Show a) => a -> ShowS
sp = showsPrec 0

unv :: TypeV -> Word8
unv (U8v v) = v
unv _ = undefined

showArray :: Int -> [TypeV] -> ShowS
showArray i l =
    let i' = i + 1 in
    ss "[\n"
    . foldl (\s v -> s . showIndent i' . showDiag i' v . showChar '\n')
            (ss "")
            l
    . showIndent i . ss "]"

showDiag :: Int -> TypeV -> ShowS
showDiag _ (U8v w) = sp w . ss "'u8"
showDiag _ (U16v w) = sp w . ss "'u16"
showDiag _ (U32v w) = sp w . ss "'u32"
showDiag _ (U64v w) = sp w . ss "'u64"
showDiag _ (I8v i) = sp i . ss "'i8"
showDiag _ (I16v i) = sp i . ss "'i16"
showDiag _ (I32v i) = sp i . ss "'i32"
showDiag _ (I64v i) = sp i . ss "'i64"
showDiag _ (F32v f) = sp f . ss "'f32"
showDiag _ (F64v f) = sp f . ss "'f64"
showDiag _ (UVv w) = sp w
showDiag i (Tuplev t) = showBlock i showDiag "{" t "}"
showDiag i (Unionv s n v) =
    ss (maybe "" (flip (++) ": ") s)
    . sp n . showChar '@' . showDiag i v
showDiag _ (Arrayv []) = ss "[ ]"
showDiag i (Arrayv l@(U8v f:r)) =
    let str = utf8decode $ f : fmap unv r in
    case str of
      Just s -> showChar '"' . ss s . showChar '"'
      Nothing -> showArray i l
showDiag i (Arrayv l) = showArray i l

instance Show TypeV where
  showsPrec _ a = showDiag 0 a

(<:<) :: Char -> Maybe String -> Maybe String
(<:<) _ Nothing = Nothing
(<:<) c (Just s)
  | isPrint c = Just (c:s)
  | otherwise = Nothing

-- Adapted from the utf8-string package
utf8decode :: [Word8] -> Maybe String
utf8decode [] = Just ""
utf8decode (c:cs)
    | c < 0x80 = chr (fromEnum c) <:< utf8decode cs
    | c < 0xc0 = Nothing
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf  0x800
    | c < 0xf8 = multi_byte 3 0x7  0x10000
    | c < 0xfc = multi_byte 4 0x3  0x200000
    | c < 0xfe = multi_byte 5 0x1  0x4000000
    | otherwise = Nothing
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|. fromEnum (c1 .&. 0x3f) in
        if d >= 0x000080
            then toEnum d <:< utf8decode ds
            else Nothing
      _ -> Nothing

    multi_byte :: Int -> Word8 -> Int -> Maybe [Char]
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux :: Int -> [Word8] -> Int -> Maybe String
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc <:< utf8decode rs
          | otherwise = Nothing

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ _ _ = Nothing

