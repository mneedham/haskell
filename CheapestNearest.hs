import Data.List
import Data.Ord

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
                testCase "shortest local tail next" test_shortest_local_tail,
                testCase "shortest back haul next" test_shortest_back_haul,
                testCase "non shortlisted sorted by these criteria too" test_non_shortlisted
            ]
    ] 


data Row = Row { shortListed :: Bool, cost :: Float, localTailDistance :: Int, backhaulDistance :: Int } deriving (Show, Eq)

compareRow :: Row -> Row -> Ordering
compareRow x y = if comparing (not . shortListed) x y == EQ then 
					if comparing cost x y == EQ then
						if comparing localTailDistance x y == EQ then
							comparing backhaulDistance x y
						else
							comparing localTailDistance x y
					else
						comparing cost x y
				 else 
				 	comparing (not . shortListed) x y


shortListedRow = Row {shortListed = True, cost = 10.0, localTailDistance = 20, backhaulDistance = 30 }
nonShortListedRow = Row {shortListed = False, cost = 10.0, localTailDistance = 20, backhaulDistance = 30 }

test_shortlisted_first = sortBy compareRow [nonShortListedRow, shortListedRow] @?= [shortListedRow, nonShortListedRow]


shortListedCheapRow = Row {shortListed = True, cost = 10.0, localTailDistance = 20, backhaulDistance = 30 }
shortListedExpensiveRow = Row {shortListed = True, cost = 50.0, localTailDistance = 20, backhaulDistance = 30 }

test_cheapest_next = sortBy compareRow [nonShortListedRow, shortListedExpensiveRow, shortListedCheapRow] @?= 
					 [shortListedCheapRow, shortListedExpensiveRow, nonShortListedRow]


slCheapCloseRow = Row {shortListed = True, cost = 10.0, localTailDistance = 1, backhaulDistance = 30 }
slCheapFarAwayRow = Row {shortListed = True, cost = 10.0, localTailDistance = 100, backhaulDistance = 30 }
slExpensiveCloseRow = Row {shortListed = True, cost = 50.0, localTailDistance = 1, backhaulDistance = 30 }
slExpensiveFarAwayRow = Row {shortListed = True, cost = 50.0, localTailDistance = 100, backhaulDistance = 30 }
nslCheapRow = Row {shortListed = False, cost = 5.0, localTailDistance = 50, backhaulDistance = 30 }

test_shortest_local_tail = sortBy compareRow [nslCheapRow, slExpensiveCloseRow, slExpensiveFarAwayRow, slCheapFarAwayRow, slCheapCloseRow] @?= 
					      [slCheapCloseRow, slCheapFarAwayRow, slExpensiveCloseRow, slExpensiveFarAwayRow, nslCheapRow]


slbackHaulNear = Row {shortListed = True, cost = 10.0, localTailDistance = 1, backhaulDistance = 30 }
slbackHaulFarAway = Row {shortListed = True, cost = 10.0, localTailDistance = 1, backhaulDistance = 50 }
slbackHaulFarAway2 = Row {shortListed = True, cost = 10.0, localTailDistance = 1, backhaulDistance = 45 }
nslBackHaulNear= Row {shortListed = False, cost = 5.0, localTailDistance = 50, backhaulDistance = 30 }
test_shortest_back_haul = sortBy compareRow [nslBackHaulNear, slbackHaulNear, slbackHaulFarAway, slbackHaulFarAway2] @?= 
                          [slbackHaulNear,slbackHaulFarAway2, slbackHaulFarAway, nslBackHaulNear]


cheap = Row {shortListed = False, cost = 5.0, localTailDistance = 50, backhaulDistance = 30 }
expensiveNear = Row {shortListed = False, cost = 50.0, localTailDistance = 50, backhaulDistance = 30 }
expensiveFarLocalNearBack = Row {shortListed = False, cost = 50.0, localTailDistance = 500, backhaulDistance = 30 }
expensiveFarLocalFarBack = Row {shortListed = False, cost = 50.0, localTailDistance = 500, backhaulDistance = 50 }

test_non_shortlisted = sortBy compareRow [expensiveFarLocalFarBack, expensiveNear, expensiveFarLocalNearBack, cheap] @?= 
                       [cheap, expensiveNear, expensiveFarLocalNearBack, expensiveFarLocalFarBack]                          