let rec ft_power (n: int, p: int) : int =
        if p = 0 then 1
        else n * (ft_power (n, p - 1))
