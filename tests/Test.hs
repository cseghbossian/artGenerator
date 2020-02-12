import Test.Tasty
import Common
import TailRecursion
import RandomArt

main :: IO ()
main = runTests [ unit2 ]

unit2 :: Score -> TestTree
unit2 sc = testGroup "Unit 1"
  [ mkTest
      (assoc 0 "william")
      [("ranjit", 85), ("william",23), ("moose",44)]
      23
      "assoc 0"
  , mkTest
      (assoc 0 "ranjit")
      [("ranjit", 85), ("william",23), ("moose",44)]
      85
      "assoc 1"
  , mkTest
      (assoc 0 "ranjit")
      [("ranjit", 85), ("william",23), ("moose",44), ("ranjit",9)]
      85
      "assoc 2"
  , mkTest
      (assoc 0 "bob")
      [("ranjit", 85), ("william",23), ("moose",44), ("ranjit",9)]
      0
      "assoc 3"
  , mkTest
      removeDuplicates
      [1,6,2,4,12,2,13,12,6,9,13]
      [1,6,2,4,12,13,9]
      "removeDups 0"
  , mkTest
      removeDuplicates
      [1,1,1]
      [1]
      "removeDups 1"
  , mkTest
      (wwhile (\x ->  let xx = x*x*x in (xx < 100, xx)))
      2
      512
      "wwhile 1"
  , mkTest
      (fixpointL collatz)
      1
      [1]
      "fixpointL 1"
  , mkTest
      (fixpointL collatz)
      2
      [2,1]
      "fixpointL 2"
  , mkTest
      (fixpointL collatz)
      3
      [3,10,5,16,8,4,2,1]
      "fixpointL 3"
  , mkTest
      (fixpointL collatz)
      4
      [4,2,1]
      "fixpointL 4"
  , mkTest
      (fixpointL collatz)
      5
      [5,16,8,4,2,1]
      "fixpointL 5"
  , mkTest
      (fixpointL g)
      0
      [0,1000000, 540302, 857553, 654289, 793480,701369,
       763959,722102,750418,731403,744238,735604,741425,
       737506,740147,738369,739567,738760,739304,738937,
       739184,739018,739130,739054,739106,739071,739094,
       739079,739089,739082,739087,739083,739086,739084,739085]
      "fixpointL 6"
  , mkTest
      (fixpointW collatz)
      1
      1
      "fixpointW 1"
  , mkTest
      (fixpointW collatz)
      2
      1
      "fixpointW 2"
  , mkTest
      (fixpointW collatz)
      3
      1
      "fixpointW 3"
  , mkTest
      (fixpointW collatz)
      4
      1
      "fixpointW 4"
  , mkTest
      (fixpointW collatz)
      5
      1
      "fixpointW 5"
  , mkTest
      (fixpointW g)
      0
      739085
      "fixpointL 6"
  , mkTestIO
      (emitGray "sample.png" 150)
      sampleExpr3
      ()
      "eval 1"
  , mkTestIO
      (emitGray "sample2.png" 150)
      sampleExpr2
      ()
      "eval 2"
  , mkTestIO
      (emitRandomGray 150)
      g1
      ()
      "gray 1"
  , mkTestIO
      (emitRandomGray 150)
      g2
      ()
      "gray 2"
  , mkTestIO
      (emitRandomGray 150)
      g3
      ()
      "gray 3"
  , mkTestIO
      (emitRandomColor 150)
      c1
      ()
      "color 1"
  , mkTestIO
      (emitRandomColor 150)
      c2
      ()
      "color 2"
  , mkTestIO
      (emitRandomColor 150)
      c3
      ()
      "color 3"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (return . f)

    mkTestIO :: (Show b, Eq b) => (a -> IO b) -> a -> b -> String -> TestTree
    mkTestIO = mkTest' sc
