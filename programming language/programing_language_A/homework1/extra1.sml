fun alternate xs = 
    case xs of
        [] => 0
      | x::xs' => x - alternate xs'

(* 默认有xs存在 *)
fun min_max xs =
    case xs of
        [x] => (x, x)
      | x::other => let
                        val (min, max) = min_max other
                    in 
                        if x < min
                        then (x, max)
                        else if x > max
                        then (min, x)
                        else (min, max)
                    end


fun cumsum xs =
    case xs of
        [] => []
      | [x] => [x]
      | x::other => x::cumsum ((x + hd other)::tl other)

      (* | x::other => let val ans = cumsum other
                    in x::(List.map (fn a => a+x) ans)
                    end *)

fun greeting who = 
    case who of
        NONE => "Hello there, you!"
      | SOME name => "Hello there, " ^ name ^ "!"


fun zip (xs, ys) = 
    case xs of
        NONE => []
      | x::xs' => case ys of
                        [] => NONE
                      | y::ys' => (x, y) :: zip(xs', ys')