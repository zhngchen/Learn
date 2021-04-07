(* 可以不使用hlep funtion, 专心case即可 *)
fun get_substitutions1 (substitutions,str) =
    case substitutions of
	      [] => []
      | x::xs => case all_except_option(str,x) of
		                 NONE => get_substitutions1(xs,str)
		               | SOME y => y @ get_substitutions1(xs,str)

fun get_substitutions2 (substitutions,str) =
    let fun loop (acc,substs_left) =
        case substs_left of
            [] => acc
          | x::xs => loop ((case all_except_option(str,x) of
                                NONE => acc
                              | SOME y => acc @ y),
                           xs)
                           (* acc @ y 与 y @ acc的区别是什么呢？ *)
    in
        loop ([],substitutions)
    end

(* 更好的风格 *)
fun get_substitutions2 (substitutions,str) =
    let fun loop (acc,substs_left) =
        case substs_left of
            [] => acc
          | x::xs => case all_except_option(str, x) of
                                NONE => loop(acc, xs)
                              | SOME y => loop(acc @ y, xs)
    in
        loop ([],substitutions)
    end

(* 只是因为一个元素用[_]而不是，_ :: [] *)
fun all_same_color cs = 
    case cs of
        [] => true
      | [_] => true
      | head::neck::tail => card_color head = card_color neck 
			    andalso all_same_color(neck::tail)