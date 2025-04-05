type Bits = [Bool]
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN n char = toEnum rotation
  where halfAlphabet = n `div` 2
        offset = fromEnum char + halfAlphabet
        rotation = offset `mod` n

maxCharNumber :: Int
maxCharNumber = fromEnum (maxBound :: Char)

rotCharEncoder :: Char -> Char
rotCharEncoder char = rotN n char
  where n = 1 + maxCharNumber

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L2, L3, L1]

fourLetterAlphabetMsgEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetMsgEncoder msg = map rot4LA msg
  where n = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4LA = rotN n

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n char = toEnum rotation
  where halfAlphabet = n `div` 2
        offset = if even 2
                 then fromEnum char + halfAlphabet
                 else 1 + fromEnum char + halfAlphabet
        rotation = offset `mod` n

rotCharDecoder :: Char -> Char
rotCharDecoder char = rotNdecoder n char
  where n = 1 + maxCharNumber

msgEncoder :: String -> String
msgEncoder msg = map rotCharEncoder msg

msgDecoder :: String -> String
msgDecoder msg = map rotCharDecoder msg

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor lst1 lst2 = map xorPair (zip lst1 lst2)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (rem == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where rem = n `mod` 2
        nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse intToBits' n
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

numIntoBits [] = []
numIntoBits (b:bits) = (if b then 1 else 0) : numIntoBits bits

numFromBits [] = 0
numFromBits ((x, y):bits) = x * 2 ^ y + (numFromBits bits)

bitsToInt :: Bits -> Int
bitsToInt bits = numFromBits bitsWithPows
  where bitsWithPows = zip (numIntoBits bits) [(length bits) - 1, (length bits) - 2 .. 0]

secretMsg :: String
secretMsg = "I'm a super spy!!!"

disposableNote :: String
disposableNote = "jkshafjsahuofhwaughlashgioioearghklasjghaguehiwjfgpajg"

disposableNoteEncode' :: String -> String -> [Bits]
disposableNoteEncode' note msg = map 
                                (\(f, s) -> f `xor` s)
                                (zip nBits mBits)
  where mBits = map charToBits msg
        nBits = map charToBits note

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits

disposableNoteEncode :: String -> String -> String
disposableNoteEncode note msg = map bitsToChar bits
  where bits = disposableNoteEncode' note msg

encodeNdecode :: String -> String
encodeNdecode = disposableNoteEncode note