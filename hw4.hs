-- m ^ 0 = 1
-- m ^ n = m * (^) m (n - 1)

replicateX 0 _ = []
replicateX n x = x : replicateX (n - 1) x