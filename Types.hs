module Types where
import Data.Word
import qualified Data.Vector as V

type Registers = [Word32]
type Array     = (Word32, V.Vector Word32)
type Arrays    = V.Vector Array
type Penta     = (Word32, Word32, Word32, Word32, Word32)
data Status    = Running Word32 | Halted deriving (Eq)

data UM32      = UM32 { stat :: Status
                      , regs :: Registers
                      , arrs :: Arrays
                      }
