import Control.Monad
import System.Random (randomIO, randomRIO)


data Place = Coordinates Float Float

instance Show Place where
  show (Coordinates i j) = show (i, j)
{-
Returns coordinates of Place
-}
getCoordinates :: Place -> (Float, Float)
getCoordinates (Coordinates x y) = (x, y)

--generateSolution :: IO Float
--generateSolution = return 

{-
Generates a list of Places with random x, y
This is basically our tour for salesman
-}
generateRandomPlace :: IO Place
generateRandomPlace = do
	x <- randomRIO (1, 10) :: IO Int
	y <- randomRIO (1, 10) :: IO Int
	return $ Coordinates (fromIntegral x :: Float)  (fromIntegral y :: Float)

{-
Generate a Tour for salesman
-}
generateTour :: IO [Place]
generateTour = sequence $ [ generateRandomPlace | i <- [1..10] ]


{-
gets a random neighbor
-}
getNeighbor :: [Place] -> IO Place
getNeighbor listPlaces = do
		randomIndex <- randomRIO (1, 10) :: IO Int
		return $ listPlaces !! randomIndex

temp = 10000
temp_min = 1


getCost :: Place -> Place -> Float
getCost p1 p2 = sqrt $ (fst x - fst y)**2 + (snd x - snd y)**2 where
	x = getCoordinates p1
	y = getCoordinates p2

{-
Acceptance Probability function
-}
acceptanceProbability :: Float -> Float -> Float -> Float
acceptanceProbability cOld cNew temp = if cNew > cOld then 1 else (exp (cNew - cOld)) / temp

-- Inner while loop using recursion
-- Skipping its type since its horrible
innerLoop func args a1 a2 0     = do
				  newPlace <- getNeighbor a1
				  return (func args a1 a2 newPlace)

innerLoop func args a1 a2 limit = do
				   newPlace <- getNeighbor a1
				   return (innerLoop (limit -1) func (func args a1 a2 a3) a1 a2 newPlace)


--outerLoop 0 func temp
-- temp = takeWhile ( > t_min) [t_init * (0.9  ** i) | i <- [1..] ]
--outerLoop temp innerLoop func (initPlace, initCost) tour  limit  = 

{-
Inner function run by innerLoop
-}
innerFunc :: (Place, Float) -> [Place] -> Float -> Place -> IO (Place, Float)
innerFunc (currentPlace, currentCost) tour temp newPlace = do
	randomP  <- randomRIO (0,1) :: IO Float
	--newPlace <- getNeighbor tour

	if ap > randomP then 
		return (newPlace, newCost) 
	else 
		return (currentPlace, currentCost) where

		newCost  = getCost currentPlace newPlace
		ap       = acceptanceProbability currentCost newCost temp


main :: IO()
main = do
	-- sequence :: monad M => [m a] -> m [a]
	-- print all random places
	tour <- generateTour
	print $ map (getCoordinates) tour
	getNeighbor tour

	return ()
