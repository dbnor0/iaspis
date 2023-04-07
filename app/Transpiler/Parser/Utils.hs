module Transpiler.Parser.Utils where


uintPredicates :: [Int -> Bool]
uintPredicates = [(>= 8), (<= 256), (== 0) . (`mod` 8)]

bytesPredicates :: [Int -> Bool]
bytesPredicates = [(>= 1), (<= 32)]

testPredicates :: a -> [a -> Bool] -> Bool
testPredicates x ps = and $ ($ x) <$> ps
