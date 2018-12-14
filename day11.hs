import Data.Char
import Data.Either
import Data.List
import Data.Maybe

import Control.Monad (msum)
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Coordinate = (Int, Int)
type Cost = Int

powerlevel :: Coordinate -> Cost
powerlevel (x, y) = digit - 5
    where rackid = x + 10
          power  = (rackid * y + input) * rackid
          digit  = (power `div` 100) `mod` 10


size = 300

powergrid = Map.fromList (zip coords powerlevels)
    where coords      = [(x, y) | x <- [1..size], y <- [1..size]]
          powerlevels = fmap powerlevel coords

getPowerLevel gsize(x, y) = sum [powergrid Map.! (x', y') | x' <- [x..x+gsize - 1], y' <- [y..y+gsize - 1]]

powerlevelgrid gsize = maximum (zip (fmap (getPowerLevel gsize) coords) coords)
    where coords = [(x, y) | x <- [1..size - gsize + 1], y <- [1..size - gsize + 1]]

input = 7400

first = print (powerlevelgrid 3)

second = do
    let valax = fmap powerlevelgrid [1..30]
    print valax
    print $ maximum $ zip valax [1..30]
