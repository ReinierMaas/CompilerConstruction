let ys = C(fn y => y + 1, fn z => z - 1, 42)
in case ys of C(f1, f2, x) => f1 (f2 x) or 0
