let k = (fn y => y + 1)
in pcase Pair(42, k) of Pair(x, y) => if true then k else (fn z => z - 1)
