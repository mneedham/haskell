msort :: [Int] -> [Int]
msort unsorted = 	
	let n = middle unsorted
	in 
		if n == 0 then unsorted 
	   	else  
			let (left, right) = splitAt n unsorted
			in merge (msort left) (msort right)
	where 
		merge [] right = right
		merge left [] = left
		merge left@(x:xs) right@(y:ys) = if x < y then x : merge xs right  else y : merge  left ys


middle :: [Int] -> Int
middle = floor . (\y -> y / 2) .  fromIntegral . length		