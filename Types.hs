module Types where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Cell = (Int, Int)

data Orixo = Orixo
    { world_map :: S.Set Cell
    , input_cells :: M.Map Cell Int
    , occupied_cells :: S.Set Cell
    } --deriving (Show)

no_cell_string :: String
no_cell_string = " "

empty_cell_string :: String
empty_cell_string = "-"

occupied_cell_string :: String
occupied_cell_string = "#"

toGrid ::
       M.Map Cell Int
    -> S.Set Cell
    -> S.Set Cell
    -> [[Maybe (Maybe (Maybe Int))]]
toGrid mci soc rel =
    let limits l = (S.findMin l, S.findMax l)
        (x, xx) = limits $ S.map fst rel
        (y, yy) = limits $ S.map snd rel
        grid = [(y', x') | let x' = [x .. xx], y' <- [y .. yy]]
     in map (\(y', l) ->
                 map
                     (\x' ->
                          if (x', y') `S.member` rel
                              then if (x', y') `S.member` soc
                                       then Just $ Just $ mci M.!? (x', y')
                                       else Just Nothing
                              else Nothing)
                     l)
            grid

fromGrid :: [[a]] -> M.Map Cell a
fromGrid lla =
    let indexedGrid = zip [0 ..] . map (zip [0 ..]) $ lla
     in M.fromList . concatMap (\(y, l) -> map (\(x, a) -> ((x, y), a)) l) $
        indexedGrid

readOrixo :: [[String]] -> Orixo
readOrixo lls =
    let gd = fromGrid . reverse $ lls
        wm = M.keysSet . M.filter (/= no_cell_string) $ gd
        ic =
            M.map read .
            M.filter (not . (`elem` [no_cell_string, empty_cell_string])) $
            gd
        oc = M.keysSet ic
     in Orixo wm ic oc

instance Show Orixo where
    show (Orixo wm ic oc) =
        unlines .
        reverse .
        map
            (concatMap
                 (maybe no_cell_string $
                  maybe empty_cell_string $ maybe occupied_cell_string show)) .
        toGrid ic oc $
        wm

empty_cells :: Orixo -> S.Set Cell
empty_cells (Orixo wm _ oc) = wm S.\\ oc

data Direction
    = R
    | L
    | U
    | D
    deriving (Show, Eq, Ord)

type Solution = [(Cell, Direction)]
