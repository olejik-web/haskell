{-# LANGUAGE OverloadedStrings #-}
import System.Environment ( getArgs )
import System.Random
import Control.Monad
import qualified Data.ByteString.Char8 as BC

type Bits = [Bool]

invertBool boolValue = if boolValue == False then True else False

invertByte byte = bitsToChar (take 8 (map invertBool (charToBits byte)))

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ (if (bitsToInt bits) < 256 then (bitsToInt bits) else (1 `div` 0))

bitsToInt :: Bits -> Int
bitsToInt bits = numFromBits bitsWithPows
  where bitsWithPows = zip (numIntoBits bits) [(length bits) - 1, (length bits) - 2 .. 0]

numIntoBits [] = []
numIntoBits (b:bits) = (if b then 1 else 0) : numIntoBits bits

numFromBits [] = 0
numFromBits ((x, y):bits) = x * 2 ^ y + (numFromBits bits)

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
  where reversedBits = reverse $ intToBits' n
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

randomInvertSection :: BC.ByteString -> IO BC.ByteString
randomInvertSection bytes = do
    let sectionSize = 45
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (invertSection start sectionSize bytes)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 45
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

invertSection :: Int -> Int -> BC.ByteString -> BC.ByteString
invertSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest 
        changed = BC.reverse (BC.map invertByte target)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest 
        changed = BC.reverse (BC.sort target)
      
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes 
  location <- randomRIO (1, bytesLength)
  chV <- randomRIO (0, 255)
  return (replaceByte location chV bytes)

intToBc :: Int -> BC.ByteString
intToBc n = BC.pack [intToChar n]

intToChar :: Int -> Char
intToChar n = toEnum transformN
  where transformN = n `mod` 255

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte pos chV bytes = mconcat [before, newCh, after]
  where (before, rest) = BC.splitAt pos bytes
        after = BC.drop 1 rest 
        newCh = intToBc chV

main::IO ()
main = do
    args <- getArgs
    let fileName = head args
    image <- BC.readFile fileName
    glitched <- foldM (\bytes f -> f bytes) image [randomInvertSection
                                                   -- randomSortSection
                                                   -- randomReplaceByte,
                                                   -- randomSortSection,
                                                   -- randomSortSection,
                                                   -- randomReplaceByte
                                                  ]
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "It's done"

{- hello = "hello"

sampleBytes :: BC.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes -}