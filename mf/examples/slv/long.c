begin
  x := 42;
  y := x * x;
  z := y / x;
  x := 0;
  y := x + z;
  y := y + x;
  x := z;
end