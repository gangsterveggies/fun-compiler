let length =
  fix \f x -> case x of
      [] -> 0
    | (x:xs) -> 1 + f xs
in
  let x = (length (2:2:[1,2,3]), length (2:2:[]))
  in
    fst x + snd x

