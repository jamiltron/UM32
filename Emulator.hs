-- file: Emulator.hs
module Main where
import Arrays
import Decode
import Types
import Control.Monad.State
import Data.Word
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Data.Bits ((.&.), complement)
import Debug.Trace
import Data.List (lines)
import System.Environment (getArgs)
import qualified Data.Vector as V

genesis = UM32 { stat = Running 0
               , regs = [0,0,0,0,0,0,0,0]
               , arrs = V.fromList [(0, V.fromList [0])]
               }

-- insert into the ith register the value v  
insertR :: Registers -> Int -> Word32 -> Registers
insertR regs i val = let (x,y) = splitAt (fi i) regs
                     in (x ++ [val] ++ drop 1 y)

fi        = fromIntegral
fi2 a b   = (fromIntegral a, fromIntegral b)
fi3 a b c = (fromIntegral a, fromIntegral b, fromIntegral c)

-- conditional move: if C != 0 then A = B
cMove :: Word32 -> Word32 -> Word32 -> StateT UM32 IO ()
cMove a b 0 = return ()
cMove a b _ = let (a',b') = fi2 a b
              in do
                (UM32 stat regs arrs) <- get
                let regs' = insertR regs a' (regs !! b')
                put (UM32 stat regs' arrs)
                return ()
  
-- indirect memory load: A = B[C]
iLoad :: Word32 -> Word32 -> Word32 -> StateT UM32 IO ()
iLoad a b c = let (a',b',c') = fi3 a b c
              in do
                (UM32 stat regs arrs) <- get
                let addr  = regs !! b'
                case arrayVal arrs addr c of
                  Nothing  -> return ()
                  Just val -> put (UM32 stat (insertR regs a' val) arrs)
                return ()
  
-- indirect memory store: A[B] = C
iStore :: Word32 -> Word32 -> Word32 -> StateT UM32 IO ()
iStore a b c = let (a',b',c') = fi3 a b c
               in do
                 (UM32 stat regs arrs) <- get
                 let addr = regs !! a'
                 case insertA arrs addr b' c' of
                   Nothing -> return ()
                   Just arrs' -> put (UM32 stat regs arrs')
                 return ()
  
-- opr: A = f (B C)
opr :: (Word32 -> Word32 -> Word32) -> Word32 -> Word32 -> Word32 -> StateT UM32 IO ()
opr f a b c = let (a',b',c') = fi3 a b c
              in do
                u <- get
                put (u {regs = insertR (regs u) a' (f b' c')})
  
  
-- not-and: A = B nand C
nand :: Word32 -> Word32 -> Word32 -> StateT UM32 IO ()
nand a b c = let a' = fromIntegral a
                 b' = fromIntegral b
                 c' = fromIntegral c 
             in do
               u <- get
               let r = (regs u)
               put (u {regs = insertR r a' (complement $ (r !! b' .&. r !! c'))})
  
-- allocate: create an array of C words and place the id in B
allocate :: Word32 -> Word32 -> StateT UM32 IO ()
allocate b c = do
  (UM32 stat regs arrs) <- get
  let arrID = minID arrs
  let regs' = insertR regs (fromIntegral b) arrID
  let arrs' = newA arrs arrID (fromIntegral c)
  put (UM32 stat regs' arrs')
  return ()
        
-- deallocate: drop an array by the given id in C
deallocate :: Word32 -> StateT UM32 IO ()
deallocate c = let c' = fromIntegral c 
               in do
                 (UM32 stat regs arrs) <- get
                 case dropA arrs (regs !! c') of
                   Nothing    -> return ()
                   Just arrs' -> put (UM32 stat regs arrs')
                 return ()
                 
printCh :: Word32 -> StateT UM32 IO ()
printCh c
  | c >= 0 && c <= 255 = do
    (UM32 s r a) <- get
    io $ print (chr (fromIntegral (r !! (fromIntegral c))))
    return ()
  | otherwise = return ()
                
getCh :: Word32 -> StateT UM32 IO ()
getCh c = do
  ch <- io $ getChar
  (UM32 stat regs arrs) <- get
  let regs' = insertR regs (fromIntegral c) (fromIntegral (ord ch))
  put (UM32 stat regs' arrs)
  return ()
  
-- halt the machine
haltM :: StateT UM32 IO ()
haltM = do
  u <- get
  put (u {stat = Halted})

-- load the array stored in B into the 0 array, set the finger to C
progLoad :: Word32 -> Word32 -> StateT UM32 IO ()
progLoad b c = let (b',c') = fi2 b c
               in do  
                 (UM32 stat regs arrs) <- get
                 case arrayByID arrs (regs !! b') of
                   Nothing  -> return ()
                   Just arr -> let arrs' = replaceA arrs 0 arr
                               in if arrs' == Nothing then return ()
                                  else put (UM32 (Running (regs !! c')) regs (fromJust arrs'))
                 return ()

-- immediately load the value in v into A
immLoad :: Word32 -> Word32 -> StateT UM32 IO ()
immLoad a v = let (a',v') = fi2 a v
              in do
                (UM32 stat regs arrs) <- get
                let regs' = insertR regs a' v
                put (UM32 stat regs' arrs)

-- execute an instruction
execute :: Word32 -> StateT UM32 IO ()
execute instr =  case decode instr of
  (0,  a, b, c, _) -> cMove  a b c
  (1,  a, b, c, _) -> iLoad  a b c
  (2,  a, b, c, _) -> iStore a b c
  (3,  a, b, c, _) -> opr    (+) a b c
  (4,  a, b, c, _) -> opr    (*) a b c
  (5,  a, b, c, _) -> opr    div a b c
  (6,  a, b, c, _) -> nand   a b c
  (7,  _, _, _, _) -> haltM
  (8,  _, b, c, _) -> allocate b c
  (9,  _, _, c, _) -> deallocate c
  (10, _, _, c, _) -> printCh c
  (11, _, _, c, _) -> getCh c
  (12, _, b, c, _) -> progLoad b c
  (13, a, _, _, v) -> immLoad a v
  _                -> return ()
    
io :: IO a -> StateT UM32 IO a
io = liftIO

code = do
  (UM32 s r a) <- get
  case s of
    Halted    -> return ()
    Running i -> do
      case arrayByID a 0 of
        Nothing  -> return ()
        Just arr -> do
          execute (getVal arr i)
          u@(UM32 s' r' a') <- get
          put u { stat = nextInstr s' }
          code
      
nextInstr :: Status -> Status
nextInstr Halted = Halted
nextInstr (Running i) = Running (i + 1)
         
main = do
  args <- getArgs
  contents <- readFile (head args)
  let l = lines contents
  let s = length l
  let arrs = V.singleton (0, V.fromList (map toBinary l))
  runStateT code (UM32 (Running 0) [0,0,0,0,0,0,0,0] arrs)
