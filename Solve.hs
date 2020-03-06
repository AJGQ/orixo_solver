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
linesFrom o@(Orixo wm ic) c = M.fromList $ zip dirs $ map ((lineFrom o) c) dirs
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

find_direction :: Cell -> S.Set Cell -> Maybe Direction
find_direction (x, y) sc
    | all (\(x', y') -> x == x' && y < y') $ S.toList sc = Just U
    | all (\(x', y') -> x == x' && y > y') $ S.toList sc = Just D
    | all (\(x', y') -> y == y' && x < x') $ S.toList sc = Just R
    | all (\(x', y') -> y == y' && x > x') $ S.toList sc = Just L
    | otherwise = Nothing

single_dependent :: Orixo -> M.Map Cell (Direction, S.Set Cell)
single_dependent o =
    let deps = dependencies o
        no_key k = M.delete k deps
     in M.filter (not . null . snd) $
        M.mapWithKey
            (\k a ->
                 (\sc ->
                      let Just d = find_direction k sc
                       in (d, sc)) $
                 (all_cells (M.fromList [(k, a)]) S.\\ all_cells (no_key k)))
            deps

--------------------------------------------------
obligated_finder :: Orixo -> M.Map Cell (Maybe Direction)
obligated_finder o@(Orixo _ ic) =
    let deps = dependencies o
        volume = (ic M.!)
     in M.mapWithKey
            (\c mds ->
                 (\ks ->
                      if S.size ks == 1
                          then Just . head . S.toList $ ks
                          else Nothing) .
                 M.keysSet . M.filter ((== volume c) . S.size) $
                 mds)
            deps

--------------------------------------------------
discovered_finder :: Orixo -> M.Map Cell (Maybe Direction)
discovered_finder o@(Orixo _ ic) =
    let mcds = single_dependent o
        volume = (ic M.!)
     in M.mapWithKey
            (\c (d, sc) ->
                 if S.size sc == volume c
                     then Just d
                     else Nothing)
            mcds

--------------------------------------------------
two_sided_impossibility_finder :: Orixo -> S.Set Cell
two_sided_impossibility_finder o =
    let deps = dependencies o
        no_key k = M.delete k deps
     in M.keysSet .
        M.filter id .
        M.mapWithKey
            (\k a ->
                 maybe True (const False) $
                 find_direction k $
                 (all_cells (M.fromList [(k, a)]) S.\\ all_cells (no_key k))) $
        deps

--------------------------------------------------
no_options :: Orixo -> S.Set Cell
no_options o@(Orixo _ ic) =
    let deps = dependencies o
        --undefined
     in M.keysSet .
        M.filter M.null .
        M.mapWithKey (\c mdsc -> M.filter (\sc -> S.size sc >= ic M.! c) mdsc) $
        deps

--------------------------------------------------
-- obligated :  when the only cells possible to slide are the same number as 
--              the volume of the starter (DONE)
-- 
-- discovered : when the single dependents are the same number as the volume
--              of the starter (DONE)
--
-- two_sided_impossibility :    when a cell as 2 single dependents or more in 
--                              different directions (DONE)
--
-- no_options : it's impossible to slide the starter in any direction (DONE)
-- 
-- 
solve :: Orixo -> Maybe Solution
solve o@(Orixo wm ic)
    | all ($ o) filters = solveChunk o
    | otherwise = Nothing
  where
    filters = [inputInMap, isChunk, sameCellNumber]

solveChunk :: Orixo -> Maybe Solution
solveChunk (Orixo wm ic) = undefined
