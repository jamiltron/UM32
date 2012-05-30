module Types where
import qualified Data.Vector as V
import           Data.Word

type Array     = (Word32, V.Vector Word32)
type Arrays    = V.Vector Array
type Penta     = (Word32, Word32, Word32, Word32, Word32)
type Registers = [Word32]
data Status    = Running Word32 | Halted deriving (Eq)
data UM32      = UM32 { stat :: Status
                      , regs :: Registers
                      , arrs :: Arrays
                      }
