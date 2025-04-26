-- import System.Random
import System.Environment
import Control.Monad
import Data.List.Split

toInts :: String -> [Int]
toInts = map read . lines

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ " |"

data TableOfSquares = TableOfSquares [Double] [Double]

showRow :: Double -> Double -> String
showRow a b = mconcat [show a, " | ", show b, "\n"]

buildSquareTable :: Double -> Double -> Double -> TableOfSquares
buildSquareTable start end step = TableOfSquares nums squares
    where nums = [start, start + step .. end]
          squares = map (^2) nums

instance Show TableOfSquares where
    show (TableOfSquares n s) = mconcat rows
      where rows = zipWith showRow n s

userData = ['1', '2', '3', '\n', '9', '1', '2', '\n']

factorial 0 = 1
factorial number = number * factorial (number - 1)

main :: IO ()
main = do
    input <- getContents
    let nums = toInts input
    print (sum nums)
    {--args <- getArgs
    let count = if length args > 0 
                then read (head args)
                else 0 :: Int
    putStrLn ("Input " ++ show count ++ " numbers")
    nums <- replicateM count getLine
    let vals = map read nums :: [Int]
    print (sum vals)--}
    -- mapM_ putStrLn args
    -- putStrLn "Input number"
    -- number <- getLine
    -- let output = show $ factorial (read number :: Integer)
    -- putStrLn output
    {-- putStrLn "Input start value"
    start <- getLine
    putStrLn "Input end value"
    end <- getLine
    putStrLn "Input step value"
    step <- getLine
    putStrLn "The table of squares: \n"
    let output = show $ buildSquareTable (read start :: Double)
                                         (read end :: Double)
                                         (read step :: Double)
    putStrLn output --}
    {-- putStrLn "Input start value"
    start <- getLine
    putStrLn "Input end value"
    end <- getLine
    putStrLn "Your random number is"
    result <- randomRIO (read start :: Int, read end :: Int)
    putStrLn (show result) --}
    -- putStrLn "Hello! What's your name?"
    -- name <- getLine
    -- putStrLn (helloPerson name)