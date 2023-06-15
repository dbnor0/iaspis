{-# LANGUAGE OverloadedStrings #-}

module Main where

import Transpiler.Iaspis.Grammar
import Test.QuickCheck
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.HUnit
import Text.Megaparsec
import Transpiler.Parser.Source
import Transpiler.Parser.Base (reserved')


main :: IO ()
main = do
  let suite = [ test_uint
              , test_address
              , test_bool
              , test_string
              , test_bytes8
              , test_bytes_dyn
              , test_mapping
              , test_array
              , test_multiplicative
              , test_additive
              , test_comparison
              , test_shift
              , test_equality
              , test_logical
              , test_bitwise
              , test_unary
              , test_binary
              ]
  counts <- traverse runTestTT suite
  print counts

test_uint :: Test
test_uint =
  let expectedOutput = Right UIntT
      actualOutput = runParser type' "" "uint"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_address :: Test
test_address =
  let expectedOutput = Right AddressT
      actualOutput = runParser type' "" "address"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_bool :: Test
test_bool =
  let expectedOutput = Right BoolT
      actualOutput = runParser type' "" "bool"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_string :: Test
test_string =
  let expectedOutput = Right (StringT Nothing)
      actualOutput = runParser type' "" "string"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_bytes8 :: Test
test_bytes8 =
  let expectedOutput = Right (BytesT 8)
      actualOutput = runParser type' "" "bytes8"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_bytes_dyn :: Test
test_bytes_dyn =
  let expectedOutput = Right (BytesDynT Nothing)
      actualOutput = runParser type' "" "bytes"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_mapping :: Test
test_mapping =
  let expectedOutput = Right (MappingT (StringT Nothing) (MappingT AddressT UIntT))
      actualOutput = runParser type' "" "mapping (string => mapping (address => uint))"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_array :: Test
test_array =
  let expectedOutput = Right (ArrayT UIntT [Just 8] Nothing)
      actualOutput = runParser type' "" "uint[8]"
  in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_multiplicative :: Test
test_multiplicative =
    let expectedOutput = Right MultiplicationOp
        actualOutput = runParser (reserved' "*" MultiplicationOp) "" "*"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_additive :: Test
test_additive =
    let expectedOutput = Right AdditionOp
        actualOutput = runParser (reserved' "+" AdditionOp) "" "+"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_comparison :: Test
test_comparison =
    let expectedOutput = Right LessThanOp
        actualOutput = runParser (reserved' "<" LessThanOp) "" "<"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_shift :: Test
test_shift =
    let expectedOutput = Right LeftShiftOp
        actualOutput = runParser (reserved' "<<" LeftShiftOp) "" "<<"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_equality :: Test
test_equality =
    let expectedOutput = Right EqualityOp
        actualOutput = runParser (reserved' "==" EqualityOp) "" "=="
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_logical :: Test
test_logical =
    let expectedOutput = Right ConjunctionOp
        actualOutput = runParser (reserved' "&&" ConjunctionOp) "" "&&"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_bitwise :: Test
test_bitwise =
    let expectedOutput = Right BitwiseConjunctionOp
        actualOutput = runParser (reserved' "&" BitwiseConjunctionOp) "" "&"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_unary :: Test
test_unary =
    let expectedOutput = Right (UnaryE ArithmeticNegationOp (LiteralE (UIntV 5)))
        actualOutput = runParser expression "" "-5"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)

test_binary :: Test
test_binary =
    let expectedOutput = Right (BinaryE AdditionOp (LiteralE (UIntV 5)) (LiteralE (UIntV 6)))
        actualOutput = runParser expression "" "5 + 6"
    in TestCase (assertEqual "Type parser" expectedOutput actualOutput)















