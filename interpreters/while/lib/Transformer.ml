type 'a t = 'a -> 'a

let of_fun f a = f a

let id a = a

let compose t1 t2 a = t2 (t1 a)

let with_fun f t a = t (f a) a

let run t a = t a
