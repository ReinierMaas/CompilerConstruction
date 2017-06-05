begin
  proc test(val x, y, res z) is
    z := x + y;
  end
  v := 2;
  w := 3;
  call test(v, w, r);
end