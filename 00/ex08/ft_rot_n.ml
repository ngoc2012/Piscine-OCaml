let ft_rot_n (n: int) (s: string): string =
  let rot (c: char): char =
    char_of_int (((int_of_char c) - 97 + n) mod 26 + 97)
  String.map rot s
