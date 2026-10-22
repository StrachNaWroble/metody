(** Abstrakcyjny transformator stanu *)

(* Typ transformatora. Jest on abstrakcyjny: oznacza to, że użytkownik
   tego modułu nie będzie widział, jak ten typ jest zaimplementowany
   w pliku .ml. Zmienna typowa 'a oznacza typ transformowanego stanu *)
type 'a t

(* Zmienia funkcję w transformator *)
val of_fun : ('a -> 'a) -> 'a t

(* Transformator identycznościowy, który nie zmienia stanu *)
val id : 'a t

(* Połącz dwa transformatory szeregowo: użyj najpierw lewego, a potem
   prawego *)
val compose : 'a t -> 'a t -> 'a t

(* Uruchom funkcję z aktualnym stanem, a następnie zbuduj transformator w
   oparciu o wynik funkcji *)
val with_fun : ('a -> 'v) -> ('v -> 'a t) -> 'a t

(* Uruchom transformator ze stanem początkowym, zwróć stan końcowy *)
val run : 'a t -> 'a -> 'a
