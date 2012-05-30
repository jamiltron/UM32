module Decode where
import           Data.Bits   (rotateL, rotateR, (.&.))
import qualified Data.Vector as V
import           Data.Word
import           Types

-- translate string to binary value
toBinary :: String -> Word32
toBinary s = binIter (reverse s) 0 0
  where binIter s i col
          | s == []       = col
          | head s == '1' = binIter (tail s) (i + 1) (col + 2 ^ i)
          | otherwise     = binIter (tail s) (i + 1) col

-- decode the word32 into five 32bit unsigned words
decode :: Word32 -> Penta
decode s = (opcode s, regA s, regB s, regC s, val s)

-- decode the opcode value from an instruction
opcode :: Word32 -> Word32
opcode i = rotateL (0xF0000000 .&. i) 4

-- decode the value in the registerA slot
regA :: Word32 -> Word32
regA i = case opcode i of
  13 -> rotateL (0xE0000000 .&. i) 7
  _  -> rotateR (0x1C0 .&. i) 6

-- decode the value in the registerB slot
regB :: Word32 -> Word32
regB i = rotateR (0x38 .&. i) 3

-- decode the value in the registerC slot
regC :: Word32 -> Word32
regC i = 0x7 .&. i

-- decode the value in the 'value' slot
val :: Word32 -> Word32
val i = 0x1FFFFFF .&. i

