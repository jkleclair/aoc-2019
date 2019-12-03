module Day3 where

-- R8,U5,L5,D3

parseDirection :: String -> (Int, Int)
parseDirection (direction:distance)
    | direction == 'R' = (distance', 0)
    | direction == 'U' = (0, distance')
    | direction == 'L' = (negate distance', 0)
    | direction == 'D' = (0, negate distance')
    where
        distance' = read distance :: Int

parseDirections :: [String] -> [(Int, Int)] -> [(Int, Int)]
parseDirections [] acc = acc
parseDirections (direction:rest) acc = parseDirections rest (acc ++ [(x1 + x2, y1 + y2)])
    where
        (x1, y1) = last acc
        (x2, y2) = parseDirection direction

calcX :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
calcX (x1, y1) (x2, y2) (x3, y3) (x4, y4) = (x, y)
    where
        -- Using integer division because intersections will always be (Int, Int)
        x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 -x2) * (x3 * y4 - y3 * x4)) `div` denominator
        y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 -y2) * (x3 * y4 - y3 * x4)) `div` denominator
        denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)