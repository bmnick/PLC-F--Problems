#light 

let countVowels inputString = inputString.ToCharArray() |> Array.map createTuple |> Array.fold addTuples (0,0,0,0,0)

let createTuple inChar = 
	match inChar with
	| 'a' -> (1,0,0,0,0)
	| 'e' -> (0,1,0,0,0)
	| 'i' -> (0,0,1,0,0)
	| 'o' -> (0,0,0,1,0)
	| 'u' -> (0,0,0,0,1)
	| _ -> (0,0,0,0,0)

let addTuples (accACount, accECount, accICount, accOCount, accUCount) (aCount, eCount, iCount, oCount, uCount) = (accACount + aCount, accECount + eCount, accICount + iCount, accOCount + oCount, accUCount + uCount)

