let length =
  fix \f x -> case x of
      list(x) -> 1 + f (snd x)
    | otherwise -> 0
in
  let x = (length (2:2:[1,2,3]), length (2:2:[]))
  in
    fst x + snd x

