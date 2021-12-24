import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parse :: T.Text -> [Int]
parse content =
    map (read . T.unpack) $
    T.splitOn (T.singleton ',') $
    content

calcFuel :: [Int] -> Int -> Int
calcFuel positions target =
    sum usages
    where
        usages = map (usage target) positions
        usage :: Int -> Int -> Int
        usage target position =
            -- (n/2) * (1+n) re-arranged to allow for integer division without remainder
            (n * (1 + n)) `div` 2
            where
                n = distance target position
                distance target position =
                    abs (position - target)

answer :: [Int] -> Int
answer input =
    minimum $ map (calcFuel input) [minX..maxX]
    where
        minX = minimum input
        maxX = maximum input

main :: IO ()
main = do
    content <- TIO.readFile "../input/day7.txt"
    let input = parse content
    putStrLn $ show $ answer input
