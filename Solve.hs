module Solve where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Types

--------------------------------------------------
inputInMap :: Orixo -> Bool
inputInMap (Orixo wm ic) =
    let inputs = M.keysSet ic
     in (== inputs) $ S.intersection wm inputs

--------------------------------------------------
neighbor :: Cell -> Cell -> Bool
neighbor (x, y) (x', y') =
    (abs (x - x') == 1 && y == y') || (x == x' && abs (y - y') == 1)

chunks :: [Cell] -> [[Cell]]
chunks [] = []
chunks [x] = [[x]]
chunks (c:t) =
    if null neighbor_chunks
        then [c] : chs
        else concat ([c] : neighbor_chunks) : far_chunks
  where
    chs = chunks t
    is_neighbor = zip chs $ map (any (neighbor c)) chs
    neighbor_chunks = map fst $ filter snd is_neighbor
    far_chunks = map fst $ filter (not . snd) is_neighbor

isChunk :: Orixo -> Bool
isChunk = (<= 1) . length . chunks . S.toList . world_map

--------------------------------------------------
sameCellNumber :: Orixo -> Bool
sameCellNumber (Orixo wm ic) =
    let map_cell_number = length wm
        input_cell_number = M.size ic + sum (map snd $ M.toList ic)
     in map_cell_number == input_cell_number

--------------------------------------------------
linesFrom :: Orixo -> Cell -> M.Map Direction (S.Set Cell)
linesFrom o@(Orixo wm ic) c =
    M.fromList $ zip dirs $ zipWith (lineFrom o) (repeat c) dirs
  where
    dirs = [U, D, L, R]

lineFrom :: Orixo -> Cell -> Direction -> S.Set Cell
lineFrom o@(Orixo wm ic) c@(x, y) d =
    S.intersection (empty_cells o) $
    S.fromList $
    (\lx ->
         if L.null lx
             then []
             else head lx) $
    filter (any (neighbor c)) $ chunks $ S.toList $ special_filter wm
  where
    special_filter =
        S.filter
            (case d of
                 U -> \(x', y') -> x' == x && y' > y
                 D -> \(x', y') -> x' == x && y' < y
                 R -> \(x', y') -> y' == y && x' > x
                 L -> \(x', y') -> y' == y && x' < x)

dependencies :: Orixo -> M.Map Cell (M.Map Direction (S.Set Cell))
dependencies o@(Orixo wm ic) =
    S.foldl'
        (\mclc c -> M.update (const $ Just $ linesFrom o c) c mclc)
        (M.fromList $ (`zip` repeat M.empty) $ M.keys ic)
        wm

--------------------------------------------------
all_cells :: M.Map Cell (M.Map Direction (S.Set Cell)) -> S.Set Cell
all_cells = S.unions . M.map (\m -> S.unions $ S.map (m M.!) $ M.keysSet m)

single_dependent :: Orixo -> M.Map Cell (S.Set Cell)
single_dependent o =
    let deps = dependencies o
        no_key k = M.delete k deps
     in M.mapWithKey
            (\k a -> all_cells (M.fromList [(k, a)]) S.\\ all_cells (no_key k))
            deps

--------------------------------------------------
solve :: Orixo -> Maybe Solution
solve o@(Orixo wm ic)
    | all ($ o) filters = solveChunk o
    | otherwise = Nothing
  where
    filters = [inputInMap, isChunk, sameCellNumber]

solveChunk :: Orixo -> Maybe Solution
solveChunk (Orixo wm ic) = undefined
