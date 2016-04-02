import Prelude hiding (concat)

concat :: [[a]] -> [a]
concat [a] = a
concat (h:t) = h ++ concat t