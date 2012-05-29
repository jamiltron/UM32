module Arrays where
import Types
import Control.Monad.State
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Vector as V

-- find an array by its identifier
arrayByID :: Arrays -> Word32 -> Maybe Array
arrayByID arrs i = V.find (\(x,xs) -> x == i) arrs
  
-- find the value at the ith index in an array
getVal :: Array -> Word32 -> Word32
getVal arr i = (snd arr) V.! (fromIntegral i)
  
-- given array id and index, return the value at that index
arrayVal :: Arrays -> Word32 -> Word32 -> Maybe Word32
arrayVal arrs i j = let arr = arrayByID arrs i
                    in if arr == Nothing then Nothing
                       else Just (getVal (fromJust arr) j)
                            
-- given an array an index, and a value, update the value in that index to the given val
updateA :: Array -> Word32 -> Word32 -> Array
updateA arr i val = (fst arr, snd arr V.// [(fromIntegral i, val)])
                            
-- create a new array given a set of arrs, and id, and a size
newA :: Arrays -> Word32 -> Word32 -> Arrays
newA arrs i size = let arr = (i, V.iterateN (fromIntegral size) (const 0) 0)
                   in V.cons arr arrs
                    
-- given an array id, index, and value, 'insert' the value into that array index
insertA :: Arrays -> Word32 -> Word32 -> Word32 -> Maybe Arrays
insertA arrs arrID i val = let idx = V.findIndex (\(x,xs) -> x == arrID) arrs
                               arr = arrayByID arrs arrID
                           in if arr == Nothing then Nothing
                              else Just (arrs V.// [(fromJust idx, updateA (fromJust arr) i val)])
                                   
-- replace an array with the provided array
replaceA :: Arrays -> Word32 -> Array -> Maybe Arrays
replaceA arrs arrID arr = case dropA arrs arrID of
  Nothing    -> Nothing
  Just arrs' -> Just (V.cons (0, snd arr) arrs')
                                   
-- given an array id and a vector of arrays, drop the array with the given id
dropA :: Arrays -> Word32 -> Maybe Arrays
dropA arrs arrID = case V.findIndex (\(x,xs) -> x == arrID) arrs of
  Nothing  -> Nothing
  Just idx -> let (x,y) = V.splitAt idx arrs
              in Just (x V.++ V.drop 1 y)
                  
                                                
-- generate the minimum id in a list of registers
minID :: Arrays -> Word32
minID xs = min' 0 (V.length xs, xs)

-- conquer and divide minimum id strategy
min' :: Int -> (Int, Arrays) -> Word32
min' a (n, xs)
  | n == 0     = fromIntegral a
  | m == b - a = min' b (n - m, vs)
  | otherwise  = min' a (m, us)
  where (us,vs) = V.partition (\(x,y) -> fromIntegral x < b) xs
        b       = a + 1 + n `div` 2
        m       = V.length us
  
