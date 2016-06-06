let fact =
  fix \f n -> ifzero n
    then 1
    else (n * f (n - 1))
in
  fact 5

-- Tests the base implementation
