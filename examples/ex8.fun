let length =
  fix \f x -> case x of
      [] -> 0
    | (x:xs) -> 1 + f xs
in
let append =
  fix \f x y -> case x of
      [] -> y
    | (x:xs) -> (x:(f xs y))
in
let map =
  fix \f g x -> case x of
      [] -> []
    | (x:xs) -> ((g x):(f g xs))
in
let filter =
  fix \f g x -> case x of
      [] -> []
    | (x:xs) -> ifzero (g x) then (x:(f g xs)) else (f g xs)
in
let head =
  \x -> case x of
    (x:xs) -> x
in
let tail =
  \x -> case x of
    (x:xs) -> xs
in
let not = \x ->
  case x of
     0 -> 1
   | _ -> 0
in
  (length [1,2]) + head (tail (filter (\x -> not (x - 4)) (append [4,4] (map (\x -> x + 1) [1,2,3]))))

-- Stress test for lists, implements
-- all major list functions and uses them
