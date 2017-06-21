let ys = C(fn y => case y of C(f, x) => f x or 0, fn z => z - 1, 42)
in case ys of C(f1, f2, x) => f1 C(f2, x) or 0
