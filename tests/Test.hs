import Test.HUnit
import Text.Megaparsec

import Lib

test1 = TestCase (assertEqual "" (eval <$> (parse statement "" "True")) (Right True))
test2 = TestCase (assertEqual "" (eval <$> (parse statement "" "False")) (Right False))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]