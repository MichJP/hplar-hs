import Control.Monad (void)
import Test.HUnit
import Text.Megaparsec

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest expr expected = TestCase (assertEqual ""  (Right expected) (eval <$> (parse statement "" expr)))

test1 = TestLabel "test1" $ mkTest "True" True
test2 = TestLabel "test2" $ mkTest "False" False
test3 = TestLabel "test3" $ mkTest "  True" True
test4 = TestLabel "test4" $ mkTest "True and True" True
test5 = TestLabel "test5" $ mkTest "True and False" False
test6 = TestLabel "test6" $ mkTest "False and True" False
test7 = TestLabel "test7" $ mkTest "False and False" False
test8 = TestLabel "test8" $ mkTest "True or True" True

tests = [ test1
        , test2
        , test3
        , test4
        , test5
        , test6
        , test7
        , test8
        ]