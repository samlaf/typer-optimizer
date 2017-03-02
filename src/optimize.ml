(* optimize.ml --- A dummy optimizer  *)

open Sexp

module EL = Elexp
module EN = Env
module M = Myers
module SS = Set.Make(struct
		      type t = symbol
		      let compare = compare
		    end)

(* Vous recevez:
 * - une expression `e` de type `elexp` (défini dans elexp.ml)
 * - un contexte `ctx` qui donne la valeur associée à chaque variable
 *   du contexte.  Le contexte est représenté par une sorte de liste
 *   à accès O(log N) implémentée dans le fichier myers.ml.
 *   Chaque élément de la liste est une paire qui contient le nom
 *   (si la variable a un nom), et la valeur de la variable.
 * Malgré que les variables soient immuables, la valeur d'une variable est
 * stockée dans une "ref cell" parce que c'est la façon la plus simple
 * de gérer les définitions récursives.
 *
 * Vous devez renvoyer une nouvelle expression de type `elexp` équivalente à
 * `e` et idéalement plus simple/efficace.  *)

let optimize (ctx : (string option * (EN.value_type ref)) M.myers)
             (e : EL.elexp)
    : EL.elexp
  =
  let rec cstfold (e : EL.elexp)
	  : EL.elexp
    = match e with
    | Imm s -> e
    | Builtin((l, s)) -> e (* no idea when this is built... *)
    | Var((l,s), i) -> e
    | Cons((l, s)) -> e
    | Lambda((l,s), b) -> Lambda((l,s), cstfold b)
    | Let(l,d,b) -> Let(l, List.map (fun ((l, s), lxp)
				     -> ((l,s), cstfold lxp))
				    d, cstfold b)
    (* note that _+_ is a lib fct that calls a *)
    (* builtin. not sure how to call builtin *)
    (* directly *)
    | Call(Var((l,"_+_"),i), Imm(Integer(l2,n))::Imm(Integer(_,m))::[])
      -> Imm(Integer(l2,n+m))
    (* add more foldings here *)
    | Call(fct, args) -> Call(cstfold fct, List.map cstfold args)
  in EL.elexp_print (cstfold e) ; print_newline (); e

      
let livevars (e : EL.elexp)
    : (EL.elexp * SS.t)
  = match e with
  | Imm s -> (e, SS.empty)
  | _ -> (e, SS.empty)


		     
