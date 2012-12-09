
(* straight line program interpreter *)

(* Write an ML function (maxargs : stm -> int) that tells the maximum *)
(* number of arguments of any print statement within any subexpression *)
(* of a given statement. *)

fun max (x, y) = if x > y then x else y

fun maxargs (PrintStm args) = length args
  | maxargs (AssignStm (_, e)) = maxargs_exp e
  | maxargs (CompoundStm (s1, s2)) = max (maxargs s1, maxargs s2)
and maxargs_exp (IdExp _) = 0
  | maxargs_exp (NumExp _) = 0
  | maxargs_exp (OpExp (e1, _, e2)) = max (maxargs_exp e1, maxargs_exp e2)
  | maxargs_exp (EseqExp (s, e)) = max (maxargs s, maxargs_exp e);

val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
      CompoundStm(AssignStm("b",
          EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a",Minus,
                                          NumExp 1)],
                  OpExp(NumExp 10, Times, IdExp"a"))),
                  PrintStm[IdExp "b"]))

val 2 = maxargs prog

type table = (id * int) list

(* Write an ML function interp : stm -> unit that "interprets" a *)
(* program in this language... in a "functional" style -- without *)
(* assignment (:=) or arrays... *)

(* book specifies type of lookup to be table * id -> int, and not *)
(* table * int -> int option, so throw an exception if there's no *)
(* binding *)
exception NoSuchBinding
fun lookup (nil, x) = raise NoSuchBinding
  | lookup ((k, v)::rest, x) = if x = k then v else lookup (rest, x)

fun update (t, k, v) = (k, v)::t

fun apply (n:int, m:int, b:binop):int =
    case b
     of Plus => n + m
      | Times => n * m
      | Minus => n - m
      | Div => n div m

(* interpStm : stm * table -> table *)
fun interpStm (CompoundStm (s1, s2), t) = interpStm (s2, interpStm (s1, t))
  | interpStm (AssignStm (i, e), t) =
    let val (n:int, t':table) = interpExp (e, t) in
        update (t', i, n)
    end
  | interpStm (PrintStm args, t) =
    case args
     of nil => (print "\n"; t)
      | head::tail =>
        let val (n:int, t':table) = interpExp (head, t) in
            (print (Int.toString n); print " "; interpStm (PrintStm tail, t'))
        end
(* interpExp : exp * table -> int * table *)
and interpExp (IdExp i, t)  = (lookup (t, i), t)
  | interpExp (NumExp n, t) = (n, t)
  | interpExp (OpExp (e1, b, e2), t) =
    let val (n:int, t':table) = interpExp (e1, t)
        val (m:int, t'':table) = interpExp (e2, t')
    in
        (apply (n, m, b), t'')
    end
  | interpExp (EseqExp (s, e), t) =
    let val t':table = interpStm (s, t) in
        interpExp (e, t')
    end

fun interp (s:stm):unit = (interpStm (s, nil); ())

local val t = [("a",1)]
in
    val (1,t) = interpExp(IdExp("a"),t)
    val (2,t) = interpExp(OpExp(IdExp("a"),Plus,IdExp("a")),t)
end

local val t = [("a",1)]
      val t' = interpStm(AssignStm("a",NumExp(2)),t)
in
    val 2 = lookup(t',"a")
end
