begin
  y := 2;
  z := 1;
  while x > 0 do {
    x := 2;
    if 2 > 1 then {
      continue;
      unreachable := true;
    }
    else
      x := x - 1;
  }
  k := x;
end