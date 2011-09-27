#light 

let countVowels inputString = countVowelsTupled (0,0,0,0,0) inputString.ToCharArray()

let countVowelsTupled (aCount, eCount, iCount, oCount, uCount) charArray =
	match charArray with
	| 'a' :: tail -> countVowelsTupled (aCount + 1, eCount, iCount, oCount, uCount) tail
	| 'e' :: tail -> countVowelsTupled (aCount, eCount + 1, iCount, oCount, uCount) tail
	| 'i' :: tail -> countVowelsTupled (aCount, eCount, iCount + 1, oCount, uCount) tail
	| 'o' :: tail -> countVowelsTupled (aCount, eCount, iCount, oCount + 1, uCount) tail
	| 'u' :: tail -> countVowelsTupled (aCount, eCount, iCount, oCount, uCount + 1) tail
	| _ :: tail -> countVowelsTupled (aCount, eCount, iCount, oCount, uCount) tail
