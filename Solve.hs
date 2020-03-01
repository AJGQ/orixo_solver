module Solve where

import Types
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

--------------------------------------------------

inputInMap :: Orixo -> Bool
inputInMap (Orixo wm ic) = let
    inputs = M.keysSet ic
    in
    (== inputs) $ S.intersection wm inputs

--------------------------------------------------

neighbor :: Cell -> Cell -> Bool
neighbor (x,y) (x',y') = (abs (x-x') == 1 && y==y') || (x==x' && abs (y-y') == 1)

chunks :: [Cell] -> [[Cell]]
chunks [] = []
chunks [x] = [[x]]
chunks (c:t) = if null neighbor_chunks then [c]:chs else (concat ([c]:neighbor_chunks)) : far_chunks
    where
        chs = chunks t
        is_neighbor = zip chs $ map (or . map (neighbor c)) chs
        neighbor_chunks = map fst $ filter snd is_neighbor
        far_chunks = map fst $ filter (not . snd) is_neighbor

isChunk :: Orixo -> Bool
isChunk = (<=1) . length . chunks . S.toList . world_map

--------------------------------------------------

sameCellNumber :: Orixo -> Bool
sameCellNumber (Orixo wm ic) = let
    map_cell_number = length wm
    input_cell_number = M.size ic + (sum $ map snd $ M.toList ic)
    in
    map_cell_number == input_cell_number

--------------------------------------------------

linesFrom :: Orixo -> Cell -> M.Map Direction (S.Set Cell)
linesFrom o@(Orixo wm ic) c = M.fromList $ zip dirs $ map (uncurry (lineFrom o)) $ zip (repeat c) dirs
    where
        dirs = [U,D,L,R]

lineFrom :: Orixo -> Cell -> Direction -> S.Set Cell
lineFrom (Orixo wm ic) c@(x,y) d 
    = S.fromList 
    $ (\lx -> if L.null lx then [] else head lx) 
    $ filter (or . map (neighbor c)) 
    $ chunks 
    $ S.toList 
    $ special_filter wm
    where
        special_filter = S.filter (case d of
            U -> (\(x',y') -> x'==x && y'>y)
            D -> (\(x',y') -> x'==x && y'<y)
            R -> (\(x',y') -> y'==y && x'>x)
            L -> (\(x',y') -> y'==y && x'<x))

dependencies :: Orixo -> M.Map Cell (M.Map Direction (S.Set Cell))
dependencies o@(Orixo wm ic) = S.foldl' 
    (\mclc c -> M.update (const $ Just $ linesFrom o c) c mclc) 
    (M.fromList $ (`zip` repeat M.empty) $ M.keys ic) 
    wm

--------------------------------------------------

solve :: Orixo -> Maybe Solution
solve o@(Orixo wm ic) 
    | and $ map ($ o) filters = solve_chunk o
    | otherwise = Nothing
    where
        filters = [inputInMap, isChunk, sameCellNumber]

solve_chunk :: Orixo -> Maybe Solution
solve_chunk (Orixo wm ic) = undefined

