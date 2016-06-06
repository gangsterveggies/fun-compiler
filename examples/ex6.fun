let fact =
  fix \f n -> case n of
      0 -> 1
    | n -> (n * f (n - 1))
in
  fact 5

-- A rewrite of example 1 using
-- pattern matching
