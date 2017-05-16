begin
  y := 2;
  z := 1;
  while x > 0 do {
    break;
    x := 2;
    if 2 > 1 then {
      reachable := true;
      break;
      unreachable1 := true;
      unreachable2 := true;
    }
    else
      x := x - 1;
  }
  k := x;
end