let length =
  fix \f x -> case x of
      (x:xs) -> 1 + f xs
    | otherwise -> 0
in
  let x = (length (2:2:[1,2,3]), length (2:2:[]))
  in
    fst x + snd x

-- Tests pattern matching and lists
