type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail


fun pass_or_fail who =
    case who of
        (NONE, _) => fail
      | (SOME i, _) => if i >= 75
                       then pass
                       else fail

