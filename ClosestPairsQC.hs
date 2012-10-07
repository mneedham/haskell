import ClosestPairs

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Framework.Providers.HUnit
import Test.HUnit

main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "should not window if n is bigger than list size" prop_nothing_if_n_too_large,
                testProperty "should create sub arrays the size of window" prop_size_of_sub_arrays_is_n,
                testProperty "should group adjacent items into windows" prop_size_of_sub_arrays_is_n,                
                testCase "should window a simple list" test_should_window_simple_list
            ]
    ] 

prop_groups_adjacent_items n xs = not (null xs) ==> (last $  last $ windowed n xs) == last xs
	where types = (xs :: [Int], n :: Int)								  	

prop_nothing_if_n_too_large n xs = n > length xs ==>  windowed n xs == []
	where types = (xs :: [Int], n :: Int)

prop_size_of_sub_arrays_is_n n xs =  n > 0 ==> all (\subArray -> length subArray == n)  (windowed n xs)  
	where types = (xs :: [Int], n :: Int)
	
test_should_window_simple_list = windowed 2 [1..10] @?= [[1,2], [2,3], [3,4], [4,5], [5,6], [6,7], [7,8], [8,9], [9,10]]	


prop_dc_bf xs = (length xs > 2) (fromJust $ bfClosest xs) == dcClosest xs