def cons = fun x -> fun xs -> (x, xs)
def nil = ()
def head = fun x -> match x with (x,y) -> x
def tail = fun x -> match x with (x,y) -> y    
def is_nil = fun x -> x == ()

def rec split xs =
  if is_nil xs then (nil, nil)
  else if is_nil (tail xs) then (xs, nil)
  else
    match split (tail (tail xs)) with (left, right) ->
      (cons (head xs) left, cons (head (tail xs)) right)

def rec merge left right =
  if is_nil left then right
  else if is_nil right then left
  else if head left <= head right then
    cons (head left) (merge (tail left) right)
  else
    cons (head right) (merge left (tail right))

def rec merge_sort xs =
  if is_nil xs then nil
  else if is_nil (tail xs) then xs
  else
    match split xs with (left, right) ->
      merge (merge_sort left) (merge_sort right)

def example =
  let xs = cons 4 (cons 7 (cons 2 (cons 6 (cons 3 (cons 1 (cons 5 nil)))))) in
  merge_sort xs