import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parseSide :: T.Text -> [String]
parseSide side =
    map T.unpack $
    T.splitOn (T.singleton ' ') $
    side

parseLine :: T.Text -> [[String]]
parseLine line =
    map parseSide $
    T.splitOn (T.pack " | ") $
    line

parse :: T.Text -> [[[String]]]
parse content =
    map parseLine $
    T.lines $
    content

is1478 :: String -> Bool
is1478 digit =
    (length digit) `elem` [2, 3, 4, 7]

count1478 :: [String] -> Int
count1478 digits =
    length (filter is1478 digits)

answer input =
    sum $ map (count1478 . head . tail) input

main :: IO ()
main = do
    content <- TIO.readFile "../input/day8.txt"
    let input = parse content
    putStrLn $ show $ answer input
