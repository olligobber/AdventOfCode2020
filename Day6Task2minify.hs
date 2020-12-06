import Data.List.Split
import Data.List
main=interact$show.sum.fmap(length.foldl1 intersect.lines).splitOn"\n\n"
