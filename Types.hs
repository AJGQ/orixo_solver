module Types where

import qualified Data.List as L (null)
import qualified Data.Map  as M
import qualified Data.Set  as S

type Cell = (Int, Int)

data Orixo = Orixo
    { world_map   :: S.Set Cell
    , input_cells :: M.Map Cell Int
    } deriving (Show)

empty_cells :: Orixo -> S.Set Cell
empty_cells (Orixo wm ic) = wm S.\\ M.keysSet ic

data Direction
    = R
    | L
    | U
    | D
    deriving (Show, Eq, Ord)

type Solution = [(Cell, Direction)]

{-
 - 2##
 -}
ex_1_1 :: Orixo
ex_1_1 = Orixo (S.fromList [(0, 0), (1, 0), (2, 0)]) $ M.fromList [((0, 0), 2)]

{-
 -  1
 - 2##
 -  #
 -}
ex_1_2 :: Orixo
ex_1_2 =
    Orixo (S.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]) $
    M.fromList [((0, 1), 2), ((1, 2), 1)]

{-
 - 1##
 -  1
 -}
ex1 :: Orixo
ex1 =
    Orixo (S.fromList [(0, 1), (1, 0), (1, 1), (2, 1)]) $
    M.fromList [((0, 1), 1), ((1, 0), 1)]

{-
 -  11
 - 1###
 -}
ex2 :: Orixo
ex2 =
    Orixo (S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (1, 1), (2, 1)]) $
    M.fromList [((0, 0), 1), ((1, 1), 1), ((2, 1), 1)]
