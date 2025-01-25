let ackermann m n =
        if m = 0 then n + 1
        else if (n = 0) && (m > 0) then ackermann (m - 1) 1
        else if (n > 0) && (m > 0) then ackermann (m - 1) 1
        else -1
