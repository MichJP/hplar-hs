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

tests = [ test1
        , test2
        , test3
        , test4
        , test5
        , test6
        , test7
        , test8
        , test9
        ]