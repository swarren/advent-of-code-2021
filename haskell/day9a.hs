import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parse :: T.Text -> [[Int]]
parse content =
    map (map C.digitToInt . T.unpack) $
    T.lines $
    content

padded :: [[Int]] -> [[Int]]
padded l =
    [topRow] ++
    (map (\x -> [10] ++ x ++ [10]) l) ++
    [bottomRow]
    where
        width = length $ head l
        topRow = replicate (width + 2) 10
        bottomRow = topRow

processElem :: (Int, Int, Int, Int, Int) -> Int
processElem (above, below, left, right, elem) =
    if isLowest then (elem + 1) else 0
    where
        isLowest =
            (elem < above) &&
            (elem < below) &&
            (elem < left) &&
            (elem < right)

processRow :: ([Int], [Int], [Int]) -> Int
processRow (above, row, below) =
    sum $ map processElem $ L.zip5 (tail above) (tail below) row (drop 2 row) (tail row)

answer :: [[Int]] -> Int
answer input =
    sum $ map processRow $ zip3 p (tail p) (drop 2 p)
    where
        p = padded input

main :: IO ()
main = do
    content <- TIO.readFile "../input/day9.txt"
    let input = parse content
    putStrLn $ show $ answer input
