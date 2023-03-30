{-# LANGUAGE DerivingStrategies #-}

module Yul.Grammar where

import Data.Text as T


type Identifier = T.Text

data Statement 
  = BlockStmt [Statement]
  | VarDeclStmt Identifier Expression
  | AssignmentStmt Expression Expression
  | IfStmt Expression Statement
  | ExpressionStmt Expression
  | SwitchStmt Expression [(Identifier, Statement)]
  deriving stock (Eq, Show)

data Expression 
  = IdentifierE Identifier
  | BuiltinE Builtin
  | LiteralE Literal
  | PathE Expression Expression
  | FunctionCallE Expression [Expression]
  deriving stock (Eq, Show)

data Literal
  = StringLit T.Text
  | NumberLit Int 
  | BooleanLit Bool
  | HexLit T.Text
  deriving stock (Eq, Show)

data Builtin
  = Stop
  | Add
  | Sub
  | Mul
  | Div
  | SDiv
  | Mod
  | SMod
  | Exp
  | Not
  | Lt
  | Gt
  | SLt
  | SGt
  | Eq
  | IsZero
  | And
  | Or
  | Xor
  | Byte
  | Shl
  | Shr
  | Sar
  | AddMod
  | MulMod
  | SignExtend
  | Keccak256
  | Pop
  | MLoad
  | MStore
  | MStore8
  | SLoad
  | SStore
  | MSize
  | Gas
  | Address
  | Balance
  | SelfBalance
  | Caller
  | CallValue
  | CallDataLoad
  | CallDataSize
  | CallDataCopy
  | ExtCodeSize
  | ExtCodeCopy
  | ReturnDataSize
  | ReturnDataCopy
  | ExtCodeHash
  | Create
  | Create2
  | Call
  | CallCode
  | DelegateCall
  | StaticCall
  | Return
  | Revert
  | SelfDestruct
  | Invalid
  | Log0
  | Log1
  | Log2
  | Log3
  | Log4
  | ChainId
  | Origin
  | GasPrice
  | BlockHash
  | CoinBase
  | Timestamp
  | Number
  | Difficulty
  | GasLimit
  | BaseFee
  deriving stock (Eq, Show)