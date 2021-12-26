import Data.List as DL
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- Using a SAT solver may be overkill, but I was interested in trying one...
import qualified SAT.MiniSat as MS

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

data WireIsSegment = WireIsSegment Char Char
    deriving (Eq, Ord, Show)

wireIsSegment :: Char -> Char -> MS.Formula WireIsSegment
wireIsSegment wire segment = MS.Var (WireIsSegment wire segment)

segmentsNum0 = "abcefg"
segmentsNum1 = "cf"
segmentsNum2 = "acdeg"
segmentsNum3 = "acdfg"
segmentsNum4 = "bcdf"
segmentsNum5 = "abdfg"
segmentsNum6 = "abdefg"
segmentsNum7 = "acf"
segmentsNum8 = "abcdefg"
segmentsNum9 = "abcdfg"

segmentRulesLenN :: [Char] -> [Char] -> MS.Formula WireIsSegment
segmentRulesLenN segments wires =
    MS.All $
    [MS.ExactlyOne [wireIsSegment wire segment | segment <- segments ] | wire <- wires]

-- length 2: 1
segmentRulesLen2 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen2 wires =
    segmentRulesLenN segmentsNum1 wires

-- length 3: 7
segmentRulesLen3 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen3 wires =
    segmentRulesLenN segmentsNum7 wires

-- length 4: 4
segmentRulesLen4 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen4 wires =
    segmentRulesLenN segmentsNum4 wires

-- length 5: 2, 3, 5,
segmentRulesLen5 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen5 wires =
    MS.ExactlyOne [
        segmentRulesLenN segmentsNum2 wires,
        segmentRulesLenN segmentsNum3 wires,
        segmentRulesLenN segmentsNum5 wires
    ]

-- length 6: 0, 6, 9
segmentRulesLen6 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen6 wires =
    MS.ExactlyOne [
        segmentRulesLenN segmentsNum0 wires,
        segmentRulesLenN segmentsNum6 wires,
        segmentRulesLenN segmentsNum9 wires
    ]

-- length 7: 8
segmentRulesLen7 :: [Char] -> MS.Formula WireIsSegment
segmentRulesLen7 wires =
    segmentRulesLenN segmentsNum8 wires

segmentRules :: [Char] -> MS.Formula WireIsSegment
segmentRules wires
    | l == 2 = segmentRulesLen2 wires
    | l == 3 = segmentRulesLen3 wires
    | l == 4 = segmentRulesLen4 wires
    | l == 5 = segmentRulesLen5 wires
    | l == 6 = segmentRulesLen6 wires
    | l == 7 = segmentRulesLen7 wires
    | otherwise = MS.Yes
    where l = length wires

lineRules :: [[[Char]]] -> MS.Formula WireIsSegment
lineRules line =
    MS.All (map segmentRules uniqueSignalPatterns)
    where
        uniqueSignalPatterns = head line

lineAssignmentSolution :: [[[Char]]] -> Maybe (Map.Map WireIsSegment Bool)
lineAssignmentSolution line =
    MS.solve $ rules
    where
        uspRules = lineRules line
        oneWirePerSegment = MS.All [ MS.ExactlyOne [ wireIsSegment wire segment | segment <- ['a'..'g'] ] | wire <- ['a'..'g'] ]
        oneSegmentPerWire = MS.All [ MS.ExactlyOne [ wireIsSegment wire segment | wire <- ['a'..'g'] ] | segment <- ['a'..'g'] ]
        rules = oneWirePerSegment MS.:&&: oneSegmentPerWire MS.:&&: uspRules

genWireToSegment :: Maybe (Map.Map WireIsSegment Bool) -> Map.Map Char Char
genWireToSegment solution =
    Map.fromList [(segment, wire) | (WireIsSegment wire segment, True) <- solution']
    where
        solution' = Map.toList (Maybe.fromJust solution)

encodeNumN :: Map.Map Char Char -> [Char] -> [Char]
--encodeNumN :: t -> [b]
encodeNumN wts segments =
    map (\c -> Maybe.fromMaybe '?' (Map.lookup c wts)) segments

encodeNums :: Map.Map Char Char -> [[Char]]
encodeNums wts = [
    encodeNumN wts segmentsNum0,
    encodeNumN wts segmentsNum1,
    encodeNumN wts segmentsNum2,
    encodeNumN wts segmentsNum3,
    encodeNumN wts segmentsNum4,
    encodeNumN wts segmentsNum5,
    encodeNumN wts segmentsNum6,
    encodeNumN wts segmentsNum7,
    encodeNumN wts segmentsNum8,
    encodeNumN wts segmentsNum9
    ]

answerLine :: [[[Char]]] -> Int
answerLine line =
    num
    where
        outputPatterns = map sort $ head $ tail line
        patterns = map sort $ encodeNums $ genWireToSegment $ lineAssignmentSolution line
        findIt pattern = Maybe.fromMaybe (-1) (findIndex (== (sort pattern)) patterns)
        digits = map findIt outputPatterns 
        num = sum $ map (\(x, y) -> x * y) (zip digits [1000, 100, 10, 1])

answer :: [[[[Char]]]] -> Int
answer input =
    sum $ map answerLine input

main :: IO ()
main = do
    content <- TIO.readFile "../input/day8.txt"
    let input = parse content
    putStrLn $ show $ answer input
