module Types where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Cell = (Int, Int)

data Orixo = Orixo
    { world_map :: S.Set Cell
    , input_cells :: M.Map Cell Int
    } --deriving (Show)

no_cell_string :: String
no_cell_string = " "

cell_string :: String
cell_string = "-"

toGrid :: M.Map Cell Int -> [Cell] -> [[Maybe (Maybe Int)]]
toGrid mci rel =
    let limits l = (minimum l, maximum l)
        (x, xx) = limits $ map fst rel
        (y, yy) = limits $ map snd rel
        grid = [(y', x') | let x' = [x .. xx], y' <- [y .. yy]]
     in map (\(y', l) ->
                 map
                     (\x' ->
                          if (x', y') `elem` rel
                              then Just $ mci M.!? (x', y')
                              else Nothing)
                     l)
            grid

fromGrid :: [[a]] -> M.Map Cell a
fromGrid lla =
    let indexedGrid = zip [0 ..] . map (zip [0 ..]) $ lla
     in M.fromList . concat . map (\(y, l) -> map (\(x, a) -> ((x, y), a)) l) $
        indexedGrid

readOrixo :: [[String]] -> Orixo
readOrixo lls =
    let gd = fromGrid . reverse $ lls
        wm = M.keysSet . M.filter (/= no_cell_string) $ gd
        ic =
            M.map read . M.filter (not . (`elem` [no_cell_string, cell_string])) $
            gd
     in Orixo wm ic

instance Show Orixo where
    show (Orixo wm ic) =
        unlines .
        reverse .
        map (concat . map (maybe no_cell_string $ maybe cell_string show)) .
        toGrid ic . S.toList $
        wm

empty_cells :: Orixo -> S.Set Cell
empty_cells (Orixo wm ic) = wm S.\\ M.keysSet ic

data Direction
    = R
    | L
    | U
    | D
    deriving (Show, Eq, Ord)

type Solution = [(Cell, Direction)]

ex_1_1 :: Orixo
ex_1_1 = readOrixo 
    [   ["2", "-", "-"]
    ]
--Orixo (S.fromList [(0, 0), (1, 0), (2, 0)]) $ M.fromList [((0, 0), 2)]

ex_1_2 :: Orixo
ex_1_2 = readOrixo 
    [   [" ", "1"]
    ,   ["2", "-", "-"]
    ,   [" ", "-"]
    ]
    --Orixo (S.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]) $
    --M.fromList [((0, 1), 2), ((1, 2), 1)]

ex1 :: Orixo
ex1 = readOrixo 
    [   ["1", "-", "-"]
    ,   [" ", "1"]
    ]
    --Orixo (S.fromList [(0, 1), (1, 0), (1, 1), (2, 1)]) $
    --M.fromList [((0, 1), 1), ((1, 0), 1)]

ex2 :: Orixo
ex2 = readOrixo 
    [   [" ", "1", "1"]
    ,   ["1", "-", "-", "-"]
    ]
    --Orixo (S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (1, 1), (2, 1)]) $
    --M.fromList [((0, 0), 1), ((1, 1), 1), ((2, 1), 1)]
