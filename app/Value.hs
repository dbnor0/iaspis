module Value where

import Data.Text
import Data.Word

data Value
    = AddressV Text
    | BoolV Bool
    | BytesV Text
    | UIntV Word
    | StringV Text