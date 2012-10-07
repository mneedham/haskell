import Data.List
import Data.Ord
import Data.Monoid

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

import Test.Framework.Providers.HUnit
import Test.HUnit


main = defaultMain tests

tests = [
        testGroup "Order the rows" [               
                testCase "shortlisted rows first" test_shortlisted_first,
                testCase "cheapest rows next" test_cheapest_next,
                testCase "shortest distance 1 next" test_shortest_distance1,
                testCase "shortest distance 2 next" test_shortest_distance2,
                testCase "non shortlisted sorted by these criteria too" test_non_shortlisted
            ]
    ] 


data Row = Row { shortListed :: Bool, cost :: Float, distance1 :: Int, distance2 :: Int } deriving (Show, Eq)

compareRow2 :: Row -> Row -> Ordering
compareRow2 x y = if comparing (not . shortListed) x y == EQ then 
                  	if comparing cost x y == EQ then
						if comparing distance1 x y == EQ then
							comparing distance2 x y
						else
							comparing distance1 x y
					else
						comparing cost x y
				  else 
					comparing (not . shortListed) x y

compareRow :: Row -> Row -> Ordering
--compareRow x y = comparing (not . shortListed) x y `mappend` comparing cost x y `mappend` comparing distance1 x y `mappend` comparing distance2 x y 
--compareRow x y = mconcat [comparing (not . shortListed) x y,  comparing cost x y, comparing distance1 x y, comparing distance2 x y]
--compareRow x y = foldr (\ fn acc -> comparing fn x y `mappend` acc) mempty ([(not . shortListed), cost, distance1, distance2] :: [Row -> Ordering])
--compareRow x y = foldr (\ fn acc -> fn x y `mappend` acc) mempty [by (not . shortListed), by cost, by distance1, by distance2]
compareRow x y = mconcat $ map (\fn -> fn x y) [by (not . shortListed), by cost, by distance1, by distance2]

by :: Ord a => (b -> a) -> b -> b -> Ordering
by = comparing

shortListedRow = Row {shortListed = True, cost = 10.0, distance1 = 20, distance2 = 30 }
nonShortListedRow = Row {shortListed = False, cost = 10.0, distance1 = 20, distance2 = 30 }

test_shortlisted_first = sortBy compareRow [nonShortListedRow, shortListedRow] @?= [shortListedRow, nonShortListedRow]


shortListedCheapRow = Row {shortListed = True, cost = 10.0, distance1 = 20, distance2 = 30 }
shortListedExpensiveRow = Row {shortListed = True, cost = 50.0, distance1 = 20, distance2 = 30 }

test_cheapest_next = sortBy compareRow [nonShortListedRow, shortListedExpensiveRow, shortListedCheapRow] @?= 
					 [shortListedCheapRow, shortListedExpensiveRow, nonShortListedRow]


slCheapCloseRow = Row {shortListed = True, cost = 10.0, distance1 = 1, distance2 = 30 }
slCheapFarAwayRow = Row {shortListed = True, cost = 10.0, distance1 = 100, distance2 = 30 }
slExpensiveCloseRow = Row {shortListed = True, cost = 50.0, distance1 = 1, distance2 = 30 }
slExpensiveFarAwayRow = Row {shortListed = True, cost = 50.0, distance1 = 100, distance2 = 30 }
nslCheapRow = Row {shortListed = False, cost = 5.0, distance1 = 50, distance2 = 30 }

test_shortest_distance1 = sortBy compareRow [nslCheapRow, slExpensiveCloseRow, slExpensiveFarAwayRow, slCheapFarAwayRow, slCheapCloseRow] @?= 
					      [slCheapCloseRow, slCheapFarAwayRow, slExpensiveCloseRow, slExpensiveFarAwayRow, nslCheapRow]


slbackHaulNear = Row {shortListed = True, cost = 10.0, distance1 = 1, distance2 = 30 }
slbackHaulFarAway = Row {shortListed = True, cost = 10.0, distance1 = 1, distance2 = 50 }
slbackHaulFarAway2 = Row {shortListed = True, cost = 10.0, distance1 = 1, distance2 = 45 }
nslBackHaulNear= Row {shortListed = False, cost = 5.0, distance1 = 50, distance2 = 30 }
test_shortest_distance2 = sortBy compareRow [nslBackHaulNear, slbackHaulNear, slbackHaulFarAway, slbackHaulFarAway2] @?= 
                          [slbackHaulNear,slbackHaulFarAway2, slbackHaulFarAway, nslBackHaulNear]


cheap = Row {shortListed = False, cost = 5.0, distance1 = 50, distance2 = 30 }
expensiveNear = Row {shortListed = False, cost = 50.0, distance1 = 50, distance2 = 30 }
expensiveFarLocalNearBack = Row {shortListed = False, cost = 50.0, distance1 = 500, distance2 = 30 }
expensiveFarLocalFarBack = Row {shortListed = False, cost = 50.0, distance1 = 500, distance2 = 50 }

test_non_shortlisted = sortBy compareRow [expensiveFarLocalFarBack, expensiveNear, expensiveFarLocalNearBack, cheap] @?= 
                       [cheap, expensiveNear, expensiveFarLocalNearBack, expensiveFarLocalFarBack]                          