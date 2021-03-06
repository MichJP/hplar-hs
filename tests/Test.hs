import Control.Monad (void)
import Test.HUnit
import Text.Megaparsec

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest expr expected = TestCase (assertEqual ""  (Right expected) (eval <$> (parse statement "" expr)))

test1 = TestLabel "literal1" $ mkTest "True" True
test2 = TestLabel "literal2" $ mkTest "False" False
test3 = TestLabel "literal3" $ mkTest "  True" True
test4 = TestLabel "and1" $ mkTest "True and True" True
test5 = TestLabel "and2" $ mkTest "True and False" False
test6 = TestLabel "and3" $ mkTest "False and True" False
test7 = TestLabel "and4" $ mkTest "False and False" False
test8 = TestLabel "or1" $ mkTest "True or True" True
test9 = TestLabel "not1" $ mkTest "not False" True
test10 = TestLabel "pred1" $ mkTest "True and False or True and True" True
test11 = TestLabel "pred2" $ mkTest "True and False or False and True" False
test12 = TestLabel "paren1" $ mkTest "not (not True)" True
test13 = TestLabel "imp1" $ mkTest "True implies True" True
test14 = TestLabel "imp2" $ mkTest "True implies False" False
test15 = TestLabel "imp3" $ mkTest "False implies True" True
test16 = TestLabel "imp4" $ mkTest "False implies False" True
test17 = TestLabel "bicond1" $ mkTest "True iff True" True
test18 = TestLabel "bicond2" $ mkTest "True iff False" False
test19 = TestLabel "bicond3" $ mkTest "False iff True" False
test20 = TestLabel "bicond4" $ mkTest "False iff False" True

tests = [ test1
        , test2
        , test3
        , test4
        , test5
        , test6
        , test7
        , test8
        , test9
        , test10
        , test11
        , test12
        , test13
        , test14
        , test15
        , test16
        , test17
        , test18
        , test19
        , test20
        ]