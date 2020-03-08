module VPlay where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Levels
import Solve
import Types

sortCells :: Direction -> [Cell] -> [Cell]
sortCells d lc =
    case d of
        U -> L.sortOn snd lc
        D -> L.sortOn (negate . snd) lc
        L -> L.sortOn (negate . fst) lc
        R -> L.sortOn fst lc

swipe :: Cell -> Direction -> Orixo -> Orixo
swipe c d o@(Orixo wm ic oc) =
    let msc =
            S.fromList . take (ic M.! c) . sortCells d . S.toList . (M.! d) $
            dependencies o M.! c
     in Orixo wm (M.delete c ic) . S.union (S.insert c oc) $ msc

swipes :: [(Cell, Direction)] -> Orixo -> Orixo
swipes l o = foldr (uncurry swipe) o l
