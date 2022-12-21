import Control.Applicative (Applicative (liftA2))

lee :: Maybe Int -> Maybe Int -> Maybe [Int]
lee m1 m2 = liftA2 (\x y -> [x, y]) m1 m2