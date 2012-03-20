msort :: [Int] -> [Int]
msort unsorted = 	
	let n = floor (fromIntegral(length unsorted) / 2)
	in if n == 0 then unsorted 
		else  
			let (left, right) = splitAt n unsorted
			in merge (msort left) (msort right)

merge :: [Int] -> [Int] -> [Int]
merge [] right = right
merge left [] = left
merge left@(x:xs) right@(y:ys) = if x < y then x : merge xs right else y : merge left ys
		