import Windowed

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck


main = defaultMain tests

tests = [
        testGroup "windowed" [
                testProperty "should not window if n is bigger than list size" prop_nothing_if_n_too_large
            ]
    ] 
						  	

prop_nothing_if_n_too_large n xs = n > length xs ==>  windowed n xs == []
    where types = (xs :: [Int], n :: Int)

