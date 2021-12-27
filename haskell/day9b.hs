import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as MB
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

parseLine :: Int -> T.Text -> [((Int, Int), Int)]
parseLine y line =
    zipWith
        (\x c -> ((y, x), (C.digitToInt c)))
        [0..]
        (T.unpack line)

parse :: T.Text -> M.Map (Int, Int) Int
parse content =
    M.fromList $
    concatMap (uncurry parseLine) $
    zip [0..] $
    lines
    where
        lines = T.lines content
        height = length lines
        width = T.length $ head lines

adjacentPoss :: (Int, Int) -> [(Int, Int)]
adjacentPoss (y, x)=
    [abovePos, belowPos, leftPos, rightPos]
    where
        abovePos = (y - 1, x)
        belowPos = (y + 1, x)
        leftPos  = (y,     x - 1)
        rightPos = (y,     x + 1)


lowestPoints :: M.Map (Int, Int) Int -> [(Int, Int)]
lowestPoints input =
    M.foldrWithKey (\k x ks -> k:ks) [] $
    M.filterWithKey isLowestPoint input
    where
        isLowestPoint :: (Int, Int) -> Int -> Bool
        isLowestPoint (y, x) elem =
            -- FIXME: Use adjacentPoss
            elem < above &&
            elem < below &&
            elem < left &&
            elem < right
            where
                above = M.findWithDefault 9 (y - 1, x)     input
                below = M.findWithDefault 9 (y + 1, x)     input
                left  = M.findWithDefault 9 (y,     x - 1) input
                right = M.findWithDefault 9 (y,     x + 1) input

destBasin :: M.Map (Int, Int) Int -> M.Map (Int, Int) (Int, Int) -> (Int, Int) -> MB.Maybe (Int, Int)
destBasin input basins (y, x)
    | elem == 9              = MB.Nothing
    | S.size destBasins /= 1 = MB.Nothing
    | otherwise              = MB.Just $ S.elemAt 0 destBasins
    where
        elem  = M.findWithDefault 9 (y, x) input
        doesFlowTo adjacentPos =
            elemAdjacent < elem
            where
                elemAdjacent = M.findWithDefault 9 adjacentPos input
        flowToPoss = filter doesFlowTo $ adjacentPoss (y, x)
        destBasins = S.fromList $ MB.mapMaybe (`M.lookup` basins) flowToPoss

expandBasins :: M.Map (Int, Int) Int -> M.Map (Int, Int) (Int, Int) -> [(Int, Int)] -> M.Map (Int, Int) (Int, Int)
expandBasins input basins [] =
    basins
expandBasins input basins (toVisit:toVisits) =
    expandBasins input basins' toVisits'
    where
        dest = destBasin input basins toVisit
        basins'
            | M.member toVisit basins = basins
            | MB.isJust dest          = M.insert toVisit (MB.fromJust dest) basins
            | otherwise               = basins
        toVisits'
            | M.member toVisit basins = toVisits
            | MB.isJust dest          = (adjacentPoss toVisit) ++ toVisits
            | otherwise               = toVisits

basinCounts :: M.Map (Int, Int) (Int, Int) -> M.Map (Int, Int) Int
basinCounts basins =
    M.foldr basinCount' M.empty basins
    where
        basinCount' :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
        basinCount' basin counts = M.insert basin (1 + M.findWithDefault 0 basin counts) counts

answer :: M.Map (Int, Int) Int -> Int
answer input =
    result
    where
        lp = lowestPoints input
        initBasins = M.fromList $ zip lp lp
        initToVist = concatMap (\(y, x) -> [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]) lp
        finalBasins = expandBasins input initBasins initToVist
        countsM = basinCounts finalBasins
        counts = M.foldr (:) [] countsM
        largest3 = take 3 $ L.sortBy (flip compare) counts
        result = product largest3

main :: IO ()
main = do
    content <- TIO.readFile "../input/day9.txt"
    let input = parse content
    print (answer input)
