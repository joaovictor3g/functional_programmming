reverse' xs = if length xs == 1 then xs else [last xs] ++ reverse' (init xs)

