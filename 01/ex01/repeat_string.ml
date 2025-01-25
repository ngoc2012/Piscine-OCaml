let repeat_string ?str:(s = "x") n =
  if n < 0 then "Error"
  else if n = 0 then ""
  else "x" ^ repeat_x (n - 1)

