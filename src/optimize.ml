(* optimize.ml --- A dummy optimizer  *)

open Sexp

module EL = Elexp
module EN = Env
module M = Myers
module SMap = Util.SMap
module Ctx = Map.Make(String)		    

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
    | Call(fct, args) ->
       (match (cstfold fct, List.map cstfold args) with
       | (Var((l,"_+_"),i), [Imm(Integer(_,n));Imm(Integer(_,m))])
	 -> Imm(Integer(l,n+m))
       | (Var((l,"_*_"),i), [Imm(Integer(_,n));Imm(Integer(_,m))])
	 -> Imm(Integer(l,n*m))
       | (Var((l,"_-_"),i), [Imm(Integer(_,n));Imm(Integer(_,m))])
	 -> Imm(Integer(l,n-m))
       | (Var((l,"_/_"),i), [Imm(Integer(_,n));Imm(Integer(_,m))])
	 -> Imm(Integer(l,n/m))
       | (Var((l,"_+_"),i), [x;Imm(Integer(_,0))]) -> x
       | (Var((l,"_+_"),i), [Imm(Integer(_,0));x]) -> x
       | (Var((l,"_*_"),i), [x;Imm(Integer(_,1))]) -> x
       | (Var((l,"_*_"),i), [Imm(Integer(_,1));x]) -> x
       | (Var((l,"_-_"),i), [x;Imm(Integer(_,0))]) -> x
       | (Var((l,"_/_"),i), [x;Imm(Integer(_,1))]) -> x

       | (Var((l,"Int_<"),i), [Imm(Integer(_,n));Imm(Integer(_,m))])
	   -> if n<m then Builtin((l, "true")) else Builtin((l, "false"))
       | _ -> Call(cstfold fct, List.map cstfold args))
    | _ -> e
    (* add more foldings here *)
    | Case (l,e,b,d) -> let getCaseBranch torf bs d =
			  if SMap.mem torf bs
			  then (match SMap.find torf bs with
				  (_,_,e) -> e)
			  else (match d with
				  Some (_,e) -> e)
			in match (cstfold e) with
			| Builtin((_, "true")) -> getCaseBranch "true" b d
			| Builtin((_, "false")) -> getCaseBranch "false" b d
			| _ -> Case(l,cstfold e,b,d)
    | Type(l) -> e (*not implemented!! *)
  and cstprop (c) (e : EL.elexp) : EL.elexp =
    match e with
    | Var((l,s), i) -> (* Ctx.iter (fun k v -> print_string k) c; *)
       if Ctx.mem s c
       then Ctx.find s c
       else e
    | Let(l,d,b) -> let d' = List.map (fun (vname,e)
				       -> (vname, cstfold (cstprop c e)))
				      d
		    in Let(l, d',
			cstprop
			  (List.fold_left
			     (fun c ((l,s),e) -> (match e with
						  | EL.Imm _  -> Ctx.add s e c
						  | _ -> c))
			     c d')
			  b)
    | Lambda(vname, b) -> Lambda(vname, cstprop c b)
    | Call(f, elst)
      -> cstfold (Call(cstprop c f, List.map (fun e -> cstprop c e) elst))
    | _ -> cstfold e
  (* in EL.elexp_print (cstprop Ctx.empty e) ; print_newline ();*)
  in cstprop Ctx.empty e (*return only e for nonoptimized version *)





		     
