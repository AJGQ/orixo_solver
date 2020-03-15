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

swipe :: Cell -> Direction -> Orixo -> Maybe Orixo
swipe c d o@(Orixo wm ic oc) =
    let sc =
            S.fromList . take (ic M.! c) . sortCells d . S.toList . (M.! d) $
            dependencies o M.! c
     in if S.size sc < ic M.! c
            then Nothing
            else Just . Orixo wm (M.delete c ic) . S.union (S.insert c oc) $ sc

swipes :: [(Cell, Direction)] -> Orixo -> Maybe Orixo
swipes l o = foldr (\(c, d) mo -> (>>= id) (swipe c d <$> mo)) (Just o) l

auto_single_dependent :: Orixo -> Orixo
auto_single_dependent o =
    let mcmd = obligated_finder o
     in if mcmd == M.empty
            then o
            else auto_single_dependent $
                 M.foldrWithKey
                     (\c md oo ->
                          case md of
                              Nothing -> oo
                              Just d -> maybe oo id $ swipe c d oo)
                     o
                     mcmd
