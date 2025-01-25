ocammlopt ft_test_sign.ml
ocamlc -o test_runner -I +unix unix.cma ft_test_sign.ml test_runner.ml && ./test_runner 