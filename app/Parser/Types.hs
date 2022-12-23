module Parser.Types where

import Data.Text
import Data.Void
import Iaspis.Grammar (Expression)
import Text.Megaparsec


type Parser = Parsec Void Text
type BinaryExpression = Expression -> Expression -> Expression
type UnaryExpression = Expression -> Expression
