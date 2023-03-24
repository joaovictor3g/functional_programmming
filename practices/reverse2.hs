import Data.List (foldl')

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []
