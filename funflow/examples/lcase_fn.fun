let ys = Cons (fn y => y + 1, Cons (fn z => z - 1, Nil))
in lcase ys of Cons(x, xs) => x or (fn k => k)
