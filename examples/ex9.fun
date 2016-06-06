# record output
let insert = fix \f x t ->
  case t of
     data Leaf () -> data Node (x, data Leaf (), data Leaf ())
   | data Node (vl, e, d) -> ifzero (x < vl)
                             then data Node (vl, (f x e), d)
                             else ifzero (x > vl)
                             then data Node (vl, e, (f x d))
                             else data Node (vl, e, d)
in
let find = fix \f x t ->
  case t of
     data Leaf () -> 0
   | data Node (vl, e, d) -> ifzero (x < vl)
                             then (f x e)
                             else ifzero (x > vl)
                             then (f x d)
                             else 1
in
let cs = (insert 5 (insert 2 (insert 8 (data Leaf ()))))
in
  {t1 = (find 1 cs);
   t2 = (find 2 cs);
   t3 = (find 3 cs);
   t4 = (find 4 cs);
   t5 = (find 5 cs);
   t6 = (find 6 cs);
   t7 = (find 7 cs);
   t8 = (find 8 cs)}

-- Ultimate stress test:
--  implements a binary search tree;
--  tests outputing a record (using a special
-- precompiler note on top);
--  tests the data constructor (that converts
-- to a 'cons Label ...');
