module DnaToRnaTest where
import Test.HUnit
import DnaToRna
import Data.Sequence

test_pattern =
  TestList [
    TestCase(assertEqual "pattern1"
      (pattern (toDna "CIIC")) (fromList [PBase 'I'], empty, empty)),
    TestCase(assertEqual "pattern2"
      (pattern (toDna "IIPIPICPIICICIIF"))
      (fromList [PBegin,
                 PSkip 2,
                 PEnd,
                 PBase 'P'], empty, empty)),
    TestCase(assertEqual "pattern3"
      (pattern (toDna "IIIICFPICFIIC"))
      (empty, fromList [toDna "ICFPICF"], empty))
    ]

test_nat =
  TestList [
    TestCase(assertEqual "nat1"
      (nat (toDna "CICFCP--"))
      (21, toDna "--"))
    ]

test_asnat =
  TestList [
    TestCase(assertEqual "asnat1"
      (asnat 21)
      (toDna "CICICP"))
    ]

test_consts =
  TestList [
    TestCase(assertEqual "consts1"
      (consts (toDna "CFPICIF"))
      (toDna "ICFP", toDna "IF")),
    TestCase(assertEqual "consts2"
      (consts (toDna ""))
      (empty, empty))
    ]

test_quote =
  TestList [
    TestCase(assertEqual "quote1"
      (quote (toDna "ICFPICFP"))
      (toDna "CFPICCFPIC"))
    ]

test_protect =
  TestList [
    TestCase(assertEqual "protect0"
      (protect 0 (toDna "ICFPICFP"))
      (toDna "ICFPICFP")),
    TestCase(assertEqual "protect1"
      (protect 1 (toDna "ICFPICFP"))
      (toDna "CFPICCFPIC")),
    TestCase(assertEqual "protect2"
      (protect 2 (toDna "ICFPICFP"))
      (toDna "FPICCFFPICCF"))
    ]

test_search =
  TestList [
    TestCase(assertEqual "search0"
      (search (toDna "ICFP") (toDna "ICFPICFP"))
      (Just 0)),
    TestCase(assertEqual "search1"
      (search (toDna "ICFP") (toDna "ICFICPIFPICFP"))
      (Just 9)),
    TestCase(assertEqual "search2"
      (search (toDna "III") (toDna "ICFPICFP"))
      Nothing)
    ]

test_replace =
  TestList [
    TestCase(assertEqual "replace-base"
      (replace (fromList [TBase 'I',
                          TBase 'C',
                          TBase 'F',
                          TBase 'P'])
               empty)
      (toDna "ICFP")),
    TestCase(assertEqual "replace-refer"
      (replace (fromList [TRefer 0 2])
               (fromList [toDna "ICFP"]))
      (toDna "FPICCF")),
    TestCase(assertEqual "replace-encode"
      (replace (fromList [TEncode 0])
               (fromList [toDna "ICFPICFPICFPICFPICFPI"]))
      (toDna "CICICP"))
    ]

test_step =
  TestList [
    TestCase(assertEqual "step1"
      (step (toDna ""))
      (empty, empty)),
    TestCase(assertEqual "step2"
      (step (toDna "IIPIPICPIICICIIFICCIFPPIICCFPC"))
      (empty, toDna "PICFC")),
    TestCase(assertEqual "step3"
      (step (toDna "IIPIPICPIICICIIFICCIFCCCPPIICCFPC"))
      (empty, toDna "PIICCFCFFPC")),
    TestCase(assertEqual "step4"
      (step (toDna "IIPIPIICPIICIICCIICFCFC"))
      (empty, toDna "I"))
    ]

test_execute =
  TestList [
    TestCase(assertEqual "execute1"
      (execute (toDna ""))
      empty)
    ]

main =
  runTestTT (test [
    TestLabel "pattern" test_pattern,
    TestLabel "nat" test_nat,
    TestLabel "asnat" test_asnat,
    TestLabel "consts" test_consts,
    TestLabel "quote" test_quote,
    TestLabel "protect" test_protect,
    TestLabel "search" test_search,
    TestLabel "replace" test_replace,
    TestLabel "step" test_step,
    TestLabel "execute" test_execute
  ])
