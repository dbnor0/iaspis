module Transpiler.Parser.Types where

import Data.Text
import Data.Void
import Transpiler.Iaspis.Grammar (Expression)
import Text.Megaparsec ( Parsec )


type Parser = Parsec Void Text
type BinaryExpression = Expression -> Expression -> Expression
type UnaryExpression = Expression -> Expression
