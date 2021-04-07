(* first class function*)
fun double x = 2*x
fun incr x = x+1
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9

(* this is much better: as always, abstract the common pieces into a function
   n_times(f,n,x) returns f(f(...(f(x)))) where there are n calls to f
   note: if we gave x type int, then we could not use this for lists
*)
fun n_times (f,n,x) = 
    if n=0
    then x
    else f (n_times(f,n-1,x))

fun increment x = x+1

fun double x = x+x

val x1 = n_times(double,4,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,12,16]) 

(* and we can define functions that use n_times *)
fun addition (n,x) = n_times(increment,n,x) (* assumes n >=0 *)
fun double_n_times (n,x) = n_times(double,n,x)

(* anonymous function *)
fun triple_n_times3 (n,x) = 
    n_times((let fun triple y = 3*y in triple end), n, x)

(* This does not work: a function /binding/ is not an /expression/ *)
(* fun triple_n_times3 (n,x) = n_times((fun triple y = 3*y), n, x) *)

(* This /anonymous function/ expression works and is the best style: *)
(* Notice the function has no name *)

fun triple_n_times4 (n,x) = n_times((fn y => 3*y), n, x)

 

(* lexical scope and clousure: function and env *)
(* map, filter, fold *)
fun map (f,xs) =
    case xs of
	[] => []
      | x::xs' => (f x)::(map(f,xs'))

val x1 = map ((fn x => x+1), [4,8,12,16])

fun filter (f,xs) =
    case xs of
	[] => []
      | x::xs' => if f x
		  then x::(filter (f,xs'))
		  else filter (f,xs')
fun is_even v = 
    (v mod 2 = 0)

fun all_even xs = 
    filter(is_even,xs)

fun fold (f,acc,xs) =
    case xs of 
	[] => acc
      | x::xs' => fold (f,f(acc,x),xs')

(* examples not using private data *)

fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)


(* curry and part application*)
(* old way to get the effect of multiple arguments *)
fun sorted3_tupled (x,y,z) = z >= y andalso y >= x

(* new way: currying *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

(* alternately: fun sorted3 x = fn y => fn z => z >= y andalso y >= x *)
fun fold f acc xs = (* means fun fold f = fn acc => fn xs => *)
  case xs of
    []     => acc
  | x::xs' => fold f (f(acc,x)) xs'
(* part application 注意sum 是function*)
val sum = fold (fn (x,y) => x+y) 0

