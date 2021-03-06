(* lexp.ml --- Lambda-expressions: the core language.

Copyright (C) 2011-2017  Free Software Foundation, Inc.

Author: Stefan Monnier <monnier@iro.umontreal.ca>
Keywords: languages, lisp, dependent types.

This file is part of Typer.

Typer is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

Typer is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  *)

module U = Util
module L = List
module SMap = U.SMap
open Fmt

open Sexp
open Pexp

open Myers
open Grammar

(* open Unify *)
module S = Subst

type vname = U.vname
type vref = U.vref

type label = symbol

type attribute_key  = (int * string)  (* rev_dbi * Var name *)
module AttributeMap = Map.Make (struct type t = attribute_key let compare = compare end)

(*************** Elaboration to Lexp *********************)

(* The scoping of `Let` is tricky:
 *
 * Since it's a recursive let, the definition part of each binding is
 * valid in the "final" scope which includes all the new bindings.
 *
 * But the type of each binding is not defined in that same scope.  Instead
 * it's defined in the scope of all the previous bindings.
 *
 * For exemple the type of the second binding of such a Let is defined in
 * the scope of the surrounded context extended with the first binding.
 * And the type of the 3rd binding is defined in the scope of the
 * surrounded context extended with the first and the second bindings.  *)

type ltype = lexp
 and subst = lexp S.subst
 and lexp =
   | Imm of sexp                        (* Used for strings, ...  *)
   | SortLevel of sort_level
   | Sort of U.location * sort
   | Builtin of vname * ltype * lexp AttributeMap.t option
   | Var of vref
   | Susp of lexp * subst  (* Lazy explicit substitution: e[σ].  *)
   (* This "Let" allows recursion.  *)
   | Let of U.location * (vname * lexp * ltype) list * lexp
   | Arrow of arg_kind * vname option * ltype * U.location * lexp
   | Lambda of arg_kind * vname * ltype * lexp
   | Call of lexp * (arg_kind * lexp) list (* Curried call.  *)
   | Inductive of U.location * label
                  * ((arg_kind * vname * ltype) list) (* formal Args *)
                  * ((arg_kind * vname option * ltype) list) SMap.t
   | Cons of lexp * symbol (* = Type info * ctor_name  *)
   | Case of U.location * lexp
             * ltype (* The type of the return value of all branches *)
             * (U.location * (arg_kind * vname option) list * lexp) SMap.t
             * (vname option * lexp) option               (* Default.  *)
   (* The `subst` only applies to the lexp associated
    * with the metavar's index (i.e. its "value"), not to the ltype.  *)
   | Metavar of int * subst * vname * ltype
 (*   (\* For logical metavars, there's no substitution.  *\)
  *   | Metavar of (U.location * string) * metakind * metavar ref
  * and metavar =
  *   (\* An uninstantiated var, along with a venv (stipulating over which vars
  *    * it should be closed), and its type.
  *    * If its type is not given, it implies its type should be a sort.  *\)
  *   | MetaUnset of (lexp option * lexp) VMap.t * ltype option * scope_level
  *   | MetaSet of lexp
  * and metakind =
  *   | MetaGraft of subst
  *   (\* Forward reference or Free var: Not known yet, but not instantiable by
  *    * unification.  *\)
  *   | MetaFoF
  * and subst = lexp VMap.t *)
 (*
  * The PTS I'm imagining looks like:
  *
  *    S = { TypeLevel, TypeOmega, Type ℓ }
  *    A = { Level : TypeLevel, Z : Level, S : Level → Level,
  *          Type : (ℓ : Level) → Type (S ℓ) }
  *    R = { (TypeLevel, Type ℓ, TypeOmega),
  *          (TypeLevel, TypeOmega, TypeOmega),
  *          (Type ℓ, TypeOmega, TypeOmega),
  *          (Type ℓ₁, Type ℓ₂, Type (max l₁ l₂) }
  *)
 and sort =
   | Stype of lexp
   | StypeOmega
   | StypeLevel
 and sort_level =
   | SLz
   | SLsucc of lexp
   | SLlub of lexp * lexp

type varbind =
  | Variable
  | ForwardRef
  | LetDef of lexp

module VMap = Map.Make (struct type t = int let compare = compare end)
type meta_subst = lexp VMap.t
type constraints  = (lexp * lexp) list

let empty_meta_subst : meta_subst = VMap.empty

let empty_constraint : constraints = []

let impossible = Imm Sexp.dummy_epsilon

let builtin_size = ref 0

(* :-( *)
let global_substitution = ref (empty_meta_subst, empty_constraint)

(********************** Hash-consing **********************)

(* let hc_table : (lexp, lexp) Hashtbl.t = Hashtbl.create 1000
 * let hc (e : lexp) : lexp =
 *   try Hashtbl.find hc_table e
 *   with Not_found -> Hashtbl.add hc_table e e; e *)

module WHC = Tweak.Make (struct type t = lexp
                                (* Using (=) instead of `compare` results
                                 * in an *enormous* slowdown.  Apparently
                                 * `compare` checks == before recursing
                                 * but (=) doesn't?  *)
                                let equal x y = (compare x y = 0)
                                let hash = Hashtbl.hash
                         end)
let hc_table : WHC.t = WHC.create 1000
let hc : lexp -> lexp = WHC.merge hc_table


let mkImm s                    = hc (Imm s)
let mkSortLevel l              = hc (SortLevel l)
let mkSort (l, s)              = hc (Sort (l, s))
let mkBuiltin (v, t, m)        = hc (Builtin (v, t, m))
let mkVar v                    = hc (Var v)
let mkLet (l, ds, e)           = hc (Let (l, ds, e))
let mkArrow (k, v, t1, l, t2)  = hc (Arrow (k, v, t1, l, t2))
let mkLambda (k, v, t, e)      = hc (Lambda (k, v, t, e))
let mkInductive (l, n, a, cs)  = hc (Inductive (l, n, a, cs))
let mkCons (t, n)              = hc (Cons (t, n))
let mkCase (l, e, rt, bs, d)   = hc (Case (l, e, rt, bs, d))
let mkMetavar (n, s, v, t)     = hc (Metavar (n, s, v, t))
let mkCall (f, es)
  = match f, es with
  | Call (f', es'), _ -> hc (Call (f', es' @ es))
  | _, [] -> f
  | _ -> hc (Call (f, es))

(********* Helper functions to use the Subst operations  *********)
(* This basically "ties the knot" between Subst and Lexp.
 * Maybe it would be cleaner to just move subst.ml into lexp.ml
 * and be done with it.  *)

let rec mkSusp e s =
  if S.identity_p s then e else
    (* We apply the substitution eagerly to some terms.
     * There's no deep technical rason for that:
     * it just seemed like a good idea to do it eagerly when it's easy.  *)
    match e with
    (* FIXME: `Builtin` shouuld be treated like `Imm`.  *)
    | Imm _ -> e
    | Builtin _ -> e
    | Susp (e, s') -> mkSusp e (scompose s' s)
    | Var (l,v) -> slookup s l v
    | Metavar (vn, s', vd, t) -> mkMetavar (vn, scompose s' s, vd, mkSusp t s)
    | _ -> hc (Susp (e, s))
and scompose s1 s2 = S.compose mkSusp s1 s2
and slookup s l v = S.lookup (fun l i -> mkVar (l, i))
                             (fun e o -> mkSusp e (S.shift o))
                             s l v
let ssink = S.sink (fun l i -> mkVar (l, i))


let rec lexp_location e =
  match e with
  | Sort (l,_) -> l
  | SortLevel (SLsucc e) -> lexp_location e
  | SortLevel (SLlub (e, _)) -> lexp_location e
  | SortLevel SLz -> U.dummy_location
  | Imm s -> sexp_location s
  | Var ((l,_),_) -> l
  | Builtin ((l, _), _, _) -> l
  | Let (l,_,_) -> l
  | Arrow (_,_,_,l,_) -> l
  | Lambda (_,(l,_),_,_) -> l
  | Call (f,_) -> lexp_location f
  | Inductive (l,_,_,_) -> l
  | Cons (_,(l,_)) -> l
  | Case (l,_,_,_,_) -> l
  | Susp (e, _) -> lexp_location e
  (* | Susp (_, e) -> lexp_location e *)
  | Metavar (_,_,(l,_), _) -> l


(********* Normalizing a term *********)

let vdummy = (U.dummy_location, "<anon>")
let maybev mv = match mv with None -> vdummy | Some v -> v

let rec push_susp e s =            (* Push a suspension one level down.  *)
  match e with
  | Imm _ -> e
  | SortLevel (SLz) -> e
  | SortLevel (SLsucc e') -> mkSortLevel (SLsucc (mkSusp e' s))
  | SortLevel (SLlub (e1, e2)) -> mkSortLevel (SLlub (mkSusp e1 s, mkSusp e2 s))
  | Sort (l, Stype e) -> mkSort (l, Stype (mkSusp e s))
  | Sort (l, _) -> e
  | Builtin _ -> e

  | Let (l, defs, e)
    -> let s' = L.fold_left (fun s (v, _, _) -> ssink v s) s defs in
      let (_,ndefs) = L.fold_left (fun (s,ndefs) (v, def, ty)
                                   -> (ssink v s,
                                      (v, mkSusp def s', mkSusp ty s) :: ndefs))
                                  (s, []) defs in
      mkLet (l, ndefs, mkSusp e s')
  | Arrow (ak, v, t1, l, t2)
    -> mkArrow (ak, v, mkSusp t1 s, l, mkSusp t2 (ssink (maybev v) s))
  | Lambda (ak, v, t, e) -> mkLambda (ak, v, mkSusp t s, mkSusp e (ssink v s))
  | Call (f, args) -> mkCall (mkSusp f s,
                             L.map (fun (ak, arg) -> (ak, mkSusp arg s)) args)
  | Inductive (l, label, args, cases)
    -> let (s, nargs) = L.fold_left (fun (s, nargs) (ak, v, t)
                                    -> (ssink v s, (ak, v, mkSusp t s) :: nargs))
                                   (s, []) args in
      let nargs = List.rev nargs in
      let ncases = SMap.map (fun args
                             -> let (_, ncase)
                                 = L.fold_left (fun (s, nargs) (ak, v, t)
                                                -> (ssink (maybev v) s,
                                                   (ak, v, mkSusp t s)
                                                   :: nargs))
                                               (s, []) args in
                               L.rev ncase)
                            cases in
      mkInductive (l, label, nargs, ncases)
  | Cons (it, name) -> Cons (mkSusp it s, name)
  | Case (l, e, ret, cases, default)
    -> mkCase (l, mkSusp e s, mkSusp ret s,
              SMap.map (fun (l, cargs, e)
                        -> let s' = L.fold_left
                                     (fun s (_,ov) -> ssink (maybev ov) s)
                                     s cargs in
                          (l, cargs, mkSusp e s'))
                       cases,
              match default with
              | None -> default
              | Some (v,e) -> Some (v, mkSusp e (ssink (maybev v) s)))
  (* Susp should never appear around Var/Susp/Metavar because mkSusp
   * pushes the subst into them eagerly.  IOW if there's a Susp(Var..)
   * or Susp(Metavar..) it's because some chunk of code should use mkSusp
   * rather than Susp.
   * But we still have to handle them here, since push_susp is called
   * in many other cases than just when we bump into a Susp.  *)
  | Susp (e,s') -> push_susp e (scompose s' s)
  | (Var _ | Metavar _) -> nosusp (mkSusp e s)

and nosusp e =                  (* Return `e` with no outermost `Susp`.  *)
  match e with
    | Susp(e, s) -> push_susp e s
    | _ -> e


(* Get rid of `Susp`ensions and instantiated `Metavar`s.  *)
let clean meta_ctx e =
  let rec clean s e = match e with
    | Imm _ -> e
    | SortLevel (SLz) -> e
    | SortLevel (SLsucc e) -> mkSortLevel (SLsucc (clean s e))
    | SortLevel (SLlub (e1, e2)) -> mkSortLevel (SLlub (clean s e1, clean s e2))
    | Sort (l, Stype e) -> mkSort (l, Stype (clean s e))
    | Sort (l, _) -> e
    | Builtin _ -> e
    | Let (l, defs, e)
      -> let s' = L.fold_left (fun s (v, _, _) -> ssink v s) s defs in
        let (_,ndefs) = L.fold_left (fun (s,ndefs) (v, def, ty)
                                     -> (ssink v s,
                                        (v, clean s' def, clean s ty) :: ndefs))
                                  (s, []) defs in
        mkLet (l, ndefs, clean s' e)
    | Arrow (ak, v, t1, l, t2)
      -> mkArrow (ak, v, clean s t1, l, clean (ssink (maybev v) s) t2)
    | Lambda (ak, v, t, e) -> mkLambda (ak, v, clean s t, clean (ssink v s) e)
    | Call (f, args) -> mkCall (clean s f,
                               L.map (fun (ak, arg) -> (ak, clean s arg)) args)
    | Inductive (l, label, args, cases)
      -> let (s, nargs) = L.fold_left (fun (s, nargs) (ak, v, t)
                                    -> (ssink v s, (ak, v, clean s t) :: nargs))
                                   (s, []) args in
      let nargs = List.rev nargs in
      let ncases = SMap.map (fun args
                             -> let (_, ncase)
                                 = L.fold_left (fun (s, nargs) (ak, v, t)
                                                -> (ssink (maybev v) s,
                                                   (ak, v, clean s t)
                                                   :: nargs))
                                               (s, []) args in
                               L.rev ncase)
                            cases in
      mkInductive (l, label, nargs, ncases)
    | Cons (it, name) -> Cons (clean s it, name)
    | Case (l, e, ret, cases, default)
      -> mkCase (l, clean s e, clean s ret,
                SMap.map (fun (l, cargs, e)
                          -> let s' = L.fold_left
                                       (fun s (_,ov) -> ssink (maybev ov) s)
                                       s cargs in
                            (l, cargs, clean s' e))
                         cases,
                match default with
                | None -> default
                | Some (v,e) -> Some (v, clean (ssink (maybev v) s) e))
    | Susp (e, s') -> clean (scompose s' s) e
    | Var _ -> if S.identity_p s then e
              else clean S.identity (mkSusp e s)
    | Metavar (idx, s', l, t)
      -> let s = (scompose s' s) in
        try clean s (VMap.find idx meta_ctx)
        with Not_found -> mkMetavar (idx, s, l, t)
  in clean S.identity e

let lexp_name e =
  match e with
    | Imm    _ -> "Imm"
    | Var    _ -> "Var"
    | Let    _ -> "let"
    | Arrow  _ -> "Arrow"
    | Lambda _ -> "lambda"
    | Call   _ -> "Call"
    | Cons   _ -> "datacons"
    | Case   _ -> "case"
    | Inductive _ -> "typecons"
    | Susp      _ -> "Susp"
    | Builtin   (_, _, None) -> "Builtin"
    | Builtin   _ -> "AttributeTable"
    | Metavar   _ -> "Metavar"
    | Sort      _ -> "Sort"
    | SortLevel _ -> "SortLevel"

let pdatacons = Pbuiltin (U.dummy_location, "datacons")
let ptypecons = Pbuiltin (U.dummy_location, "typecons")

(* ugly printing (sexp_print (pexp_unparse (lexp_unparse e))) *)
let rec lexp_unparse lxp =
  match lxp with
    | Susp _ as e -> lexp_unparse (nosusp e)
    | Imm (sexp) -> Pimm (sexp)
    | Builtin (s, _, _) -> Pbuiltin s
    | Var ((loc, name), _) -> Pvar((loc, name))
    | Cons (t, (l, name))
      -> Pcall (pdatacons, [pexp_unparse (lexp_unparse t); Symbol (l, name)])
    | Lambda (kind, vdef, ltp, body) ->
      Plambda(kind, vdef, Some (lexp_unparse ltp), lexp_unparse body)
    | Arrow (arg_kind, vdef, ltp1, loc, ltp2) ->
      Parrow(arg_kind, vdef, lexp_unparse ltp1, loc, lexp_unparse ltp2)

    | Let (loc, ldecls, body)
      -> (* (vdef * lexp * ltype) list *)
       let pdecls = List.fold_left
                      (fun acc (vdef, lxp, ltp)
                       -> Ptype (vdef, lexp_unparse ltp)
                         :: Pexpr(vdef, lexp_unparse lxp)::acc)
                      [] ldecls in
       Plet (loc, pdecls, lexp_unparse body)

    | Call(lxp, largs) -> (* (arg_kind * lexp) list *)
      let pargs = List.map (fun (kind, elem) -> lexp_unparse elem) largs in
      let sargs = List.map (fun elem -> pexp_unparse elem) pargs in
        Pcall(lexp_unparse lxp, sargs)

    | Inductive(loc, label, lfargs, ctors) ->
      (* (arg_kind * vdef * ltype) list *)
      (* (arg_kind * pvar * pexp option) list *)
      let pfargs = List.map (fun (kind, vdef, ltp) ->
        (kind, vdef, Some (lexp_unparse ltp))) lfargs in

      (* ((arg_kind * vdef option * ltype) list) SMap.t *)
      (* (symbol * (arg_kind * pvar option * pexp) list) list *)
      let ctors = List.map (fun (str, largs) ->
        let pargs = List.map (fun (kind, var, ltp) ->
          match var with
            | Some (loc, name) -> (kind, Some (loc, name), lexp_unparse ltp)
            | None             -> (kind, None, lexp_unparse ltp)) largs
          in ((loc, str), pargs)
          ) (SMap.bindings ctors) in
      Pcall (ptypecons,
             Node (Symbol label, List.map pexp_u_formal_arg pfargs)
             :: List.map (fun ((l,name) as s, types)
                          -> Node (Symbol s,
                                  List.map pexp_u_ind_arg types))
                         ctors)

    | Case (loc, target, bltp, branches, default) ->
       let bt = lexp_unparse bltp in
      let pbranch = List.map (fun (str, (loc, args, bch)) ->
        match args with
          | [] -> Ppatsym (loc, str), lexp_unparse bch
          | _  ->
             let pat_args
               = List.map (fun (kind, vdef)
                           -> match vdef with
                             | Some vdef -> Some vdef, Ppatsym vdef
                             | None -> None, Ppatany loc)
                          args
            (* FIXME: Rather than a Pcons we'd like to refer to an existing
             * binding with that value!  *)
             in (Ppatcons (Pcall (pdatacons,
                                  [pexp_unparse bt; Symbol (loc, str)]),
                           pat_args),
                 lexp_unparse bch)
        ) (SMap.bindings branches) in

      let pbranch = match default with
        | Some (v,dft) -> ((match v with
                           | None -> Ppatany loc
                           | Some vdef -> Ppatsym vdef),
                          lexp_unparse dft)::pbranch
        | None -> pbranch
        in Pcase (loc, lexp_unparse target, pbranch)

    (* FIXME: The cases below are all broken!  *)
    | Metavar (idx, subst, (loc, name), _)
      -> Pimm (Symbol (loc, "?<" ^ name ^ "-" ^ string_of_int idx
                           ^ "[" ^ subst_string subst ^ "]>"))

    | SortLevel (SLz) -> Pbuiltin (U.dummy_location, "TypeLevel.z")
    | SortLevel (SLsucc l)
      -> Pcall (Pbuiltin (lexp_location l, "TypeLevel.succ"),
               [pexp_unparse (lexp_unparse l)])
    | SortLevel (SLlub (l1, l2))
      -> Pcall (Pbuiltin (lexp_location l1, "TypeLevel.∪"),
               [pexp_unparse (lexp_unparse l1); pexp_unparse (lexp_unparse l2)])
    | Sort (l, StypeOmega) -> Pbuiltin (l, "Type_ω")
    | Sort (l, StypeLevel) -> Pbuiltin (l, "Type_ℓ")
    | Sort (l, Stype sl)
      -> Pcall (Pbuiltin (lexp_location sl, "Type_"),
               [pexp_unparse (lexp_unparse sl)])

(* FIXME: ¡Unify lexp_print and lexp_string!  *)
and lexp_string lxp = sexp_string (pexp_unparse (lexp_unparse lxp))

and subst_string s = match s with
  | S.Identity -> "Id"
  | S.Shift (s, n) -> "(↑"^ string_of_int n ^ " " ^ subst_string s ^ ")"
  | S.Cons (l, s) -> lexp_string l ^ " · " ^ subst_string s

(* ------------------------------------------------------------------------- *)
(*                                   Printing                                *)

(*  Printing Context
 * ========================================== *)

type print_context_value =
  | Bool of bool
  | Int of int
  | Expr of lexp option
  | Predtl of grammar        (* precedence table *)

type print_context = print_context_value SMap.t

let pretty_ppctx =
  List.fold_left (fun map (key, v) -> SMap.add key v map)
    SMap.empty
    [("pretty"        , Bool (true) ); (* print with new lines and indents   *)
     ("print_type"    , Bool (true) ); (* print inferred Type                *)
     ("print_dbi"     , Bool (false)); (* print dbi index                    *)
     ("indent_size"   , Int  (2)    ); (* indent step                        *)
     ("color"         , Bool (true) ); (* use console color to display hints *)
     ("separate_decl" , Bool (true) ); (* print newline between declarations *)
     ("indent_level"  , Int  (0)    ); (* current indent level               *)
     ("parent"        , Expr (None) ); (* parent expression                  *)
     ("metavar"       , Expr (None) ); (* metavar being printed              *)
     ("col_max"       , Int  (80)   ); (* col_size + col_ofsset <= col_max   *)
     ("col_size"      , Int  (0)    ); (* current column size                *)
     ("col_ofsset"    , Int  (0)    ); (* if col does not start at 0         *)
     ("print_erasable", Bool (false));
     ("print_implicit", Bool (false));
     ("grammar"       , Predtl (default_grammar))]

(* debug_ppctx is a ref so we can modify it in the REPL *)
let debug_ppctx = ref (
  List.fold_left (fun map (key, v) -> SMap.add key v map)
    pretty_ppctx
    [("pretty"        , Bool (false) );
     ("print_dbi"     , Bool (true)  );
     ("print_erasable", Bool (true));
     ("print_implicit", Bool (true));
     ("separate_decl" , Bool (false) );])

let pp_pretty ctx = match SMap.find "pretty" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_type ctx = match SMap.find "print_type" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_dbi ctx = match SMap.find "print_dbi" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_size ctx = match SMap.find "indent_size" ctx with Int i -> i | _ -> U.typer_unreachable ""
let pp_color ctx = match SMap.find "color" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_decl ctx = match SMap.find "separate_decl" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_indent ctx = match SMap.find "indent_level" ctx with Int i -> i | _ -> U.typer_unreachable ""
let pp_parent ctx = match SMap.find "parent" ctx with Expr e -> e | _ -> U.typer_unreachable ""
let pp_meta ctx = match SMap.find "metavar" ctx with Expr e -> e | _ -> U.typer_unreachable ""
let pp_grammar ctx = match SMap.find "grammar" ctx with Predtl p -> p | _ -> U.typer_unreachable ""
let pp_colsize ctx = match SMap.find "col_size" ctx with Int i -> i | _ -> U.typer_unreachable ""
let pp_colmax ctx = match SMap.find "col_max" ctx with Int i -> i | _ -> U.typer_unreachable ""
let pp_erasable ctx = match SMap.find "print_erasable" ctx with Bool b -> b | _ -> U.typer_unreachable ""
let pp_implicit ctx = match SMap.find "print_implicit" ctx with Bool b -> b | _ -> U.typer_unreachable ""

let set_col_size p ctx = SMap.add "col_size" (Int p) ctx
let add_col_size p ctx = set_col_size ((pp_colsize ctx) + p) ctx
let reset_col_size ctx = set_col_size 0 ctx
let set_parent p ctx = SMap.add "parent" (Expr (Some p)) ctx
let set_meta p ctx   = SMap.add "metavar" (Expr (Some p)) ctx
let add_indent ctx i = SMap.add "indent_level" (Int ((pp_indent ctx) + i)) ctx

let pp_append_string buffer ctx str =
  let n = (String.length str) in
    Buffer.add_string buffer str;
    add_col_size n ctx
let pp_newline buffer ctx =
  Buffer.add_char buffer '\n';
  reset_col_size ctx

let is_binary_op str =
  let c1 = String.get str 0 in
  let cn = String.get str ((String.length str) - 1) in
    if (c1 = '_') && (cn = '_') then true else false

let get_binary_op_name name =
  String.sub name 1 ((String.length name) - 2)

let rec get_precedence expr ctx =
  let lkp name = SMap.find name (pp_grammar ctx) in
  match expr with
    | Lambda _ -> lkp "lambda"
    | Case _ -> lkp "case"
    | Let _ -> lkp "let"
    | Arrow (Aexplicit, _, _, _, _) -> lkp "->"
    | Arrow (Aimplicit, _, _, _, _) -> lkp "=>"
    | Arrow (Aerasable, _, _, _, _) -> lkp "≡>"
    | Call (exp, _) -> get_precedence exp ctx
    | Builtin ((_, name), _, _)  when is_binary_op name ->
      lkp (get_binary_op_name name)
    | Var ((_, name), _) when is_binary_op name ->
      lkp (get_binary_op_name name)
    | _ -> None, None

(*  Printing Functions
 * ========================================== *)

let rec lexp_print  e = print_string (lexp_string e)
and     lexp_string e = lexp_cstring (!debug_ppctx) e

(* Context Print *)
and lexp_cprint ctx e  = print_string (lexp_cstring ctx e)
and lexp_cstring ctx e = _lexp_str ctx e

(* Implementation *)
and _lexp_str ctx (exp : lexp) : string =

    let ctx             = set_parent exp ctx in
    let inter_ctx       = add_indent ctx 1 in
    let lexp_str        = _lexp_str ctx in
    let lexp_stri idt e = _lexp_str (add_indent ctx idt) e in

    let pretty = pp_pretty ctx in
    let color  = pp_color ctx in
    let indent = pp_indent ctx in
    let isize  = pp_size ctx in

    (* colors *)
      let red     = if color then red     else "" in
      let green   = if color then green   else "" in
      let yellow  = if color then yellow  else "" in
      let magenta = if color then magenta else "" in
      let cyan    = if color then cyan    else "" in
      let reset   = if color then reset   else "" in

    let make_indent idt = if pretty then
      (make_line ' ' ((idt + indent) * isize)) else "" in

    let newline = if pretty then "\n" else " " in
    let nl = newline in

    let keyword str  = magenta ^ str ^ reset in
    let error str    = red     ^ str ^ reset in
    let tval str     = yellow  ^ str ^ reset in
    let fun_call str = cyan    ^ str ^ reset in

    let index idx =
      let _index idx = if pp_dbi ctx then ("[" ^ (string_of_int idx) ^ "]") else "" in
      let str = _index idx in if idx < 0 then (error str) else
        (green ^ str ^ reset) in

    let kind_str k = match k with
      | Aexplicit -> "->" | Aimplicit -> "=>" | Aerasable -> "≡>" in

    let kindp_str k =  match k with
      | Aexplicit -> ":" | Aimplicit -> "::" | Aerasable -> ":::" in

    let get_name fname = match fname with
      | Builtin ((_, name), _, _) -> name,  0
      | Var((_, name), idx)       -> name, idx
      | Lambda _                  -> "__",  0
      | Cons _                    -> "__",  0
      | _                         -> "__", -1  in

    match exp with
        | Imm(value) -> (match value with
            | String (_, s) -> tval ("\"" ^ s ^ "\"")
            | Integer(_, s) -> tval (string_of_int s)
            | Float  (_, s) -> tval (string_of_float s)
            | e -> sexp_string e)

        | Susp (e, s) -> _lexp_str ctx (push_susp e s)

        | Var ((loc, name), idx) -> name ^ (index idx) ;

        | Metavar (idx, subst, (loc, name), _) ->(
          (* print metavar result if any *)
          let print_meta exp =
            let meta_ctx, _ = !global_substitution in
            let ctx = set_meta exp ctx in
              _lexp_str ctx (clean meta_ctx exp) in

          match pp_meta ctx with
            | None                 -> print_meta exp
            | Some e when e != exp -> print_meta exp
            | _ ->
              "?" ^ name ^ (subst_string subst) ^ (index idx))

        | Let (_, decls, body)   ->
            (* Print first decls without indent *)
            let h1, decls, idt_lvl =
                match _lexp_str_decls inter_ctx decls with
                    | h1::decls -> h1, decls, 2
                    | _ -> "", [], 1 in

            let decls = List.fold_left (fun str elem ->
              str ^ nl ^ (make_indent 1) ^ elem ^ " ") h1 decls in

            let n = String.length decls - 2 in
            (* remove last newline *)
            let decls = (String.sub decls 0 n) in

            (keyword "let ") ^ decls ^ (keyword " in ") ^ newline ^
                (make_indent idt_lvl) ^ (lexp_stri idt_lvl body)

        | Arrow(k, Some (_, name), tp, loc, expr) ->
            "(" ^ name ^ " : " ^ (lexp_str tp) ^ ") " ^
                (kind_str k) ^ " " ^ (lexp_str expr)

        | Arrow(k, None, tp, loc, expr) ->
           "(" ^ (lexp_str tp) ^ " "
           ^ (kind_str k) ^ " " ^ (lexp_str expr) ^ ")"

        | Lambda(k, (loc, name), ltype, lbody) ->
            let arg = "(" ^ name ^ " : " ^ (lexp_str ltype) ^ ")" in

            (keyword "lambda ") ^ arg ^ " " ^ (kind_str k) ^ newline ^
                (make_indent 1) ^ (lexp_stri 1 lbody)

        | Cons(t, (_, ctor_name)) ->
            (keyword "datacons ") ^ (lexp_str t) ^ " " ^ ctor_name

        | Call(fname, args) ->
          let name, idx = get_name fname in
          let binop_str op (_, lhs) (_, rhs) =
            "(" ^ (lexp_str lhs) ^ op ^ (index idx) ^ " " ^ (lexp_str rhs) ^ ")" in

          let print_arg str (arg_type, lxp) =
            match arg_type with
              | Aerasable when pp_erasable ctx -> str ^ " " ^ (lexp_str lxp)
              | Aimplicit when pp_implicit ctx -> str ^ " " ^ (lexp_str lxp)
              | Aexplicit                      -> str ^ " " ^ (lexp_str lxp)
              | _ -> str in (

          match args with
            | [lhs; rhs]  when is_binary_op name ->
              binop_str (" " ^ (get_binary_op_name name)) lhs rhs

            | _ -> let args = List.fold_left print_arg "" args in
               "(" ^ (lexp_str fname) ^ args ^ ")")

        | Inductive (_, (_, name), [], ctors) ->
            (keyword "typecons") ^ " (" ^ name ^") " ^ newline ^
                (lexp_str_ctor ctx ctors)

        | Inductive (_, (_, name), args, ctors)
          -> let args_str
              = List.fold_left
                  (fun str (arg_kind, (_, name), ltype)
                   -> str ^ " (" ^ name ^ " " ^ (kindp_str arg_kind) ^ " "
                     ^ (lexp_str ltype) ^ ")")
                  "" args in

            (keyword "typecons") ^ " (" ^ name ^ args_str ^") " ^
              (lexp_str_ctor ctx ctors)

        | Case (_, target, _ret, map, dflt) ->(
            let str = (keyword "case ") ^ (lexp_str target) in
            let arg_str arg
              = List.fold_left (fun str v
                                -> match v with
                                  | (_ ,None) -> str ^ " _"
                                  | (_, Some (_, n)) -> str ^ " " ^ n)
                               "" arg in

            let str = SMap.fold (fun k (_, arg, exp) str ->
                str ^ nl ^ (make_indent 1) ^
                    "| " ^ (fun_call k) ^ (arg_str arg) ^ " => " ^ (lexp_stri 1 exp))
                map str in

            match dflt with
                | None -> str
                | Some (v, df) ->
                   str ^ nl ^ (make_indent 1)
                   ^ "| " ^ (match v with None -> "_" | Some (_,name) -> name)
                   ^ " => " ^ (lexp_stri 1 df))

        | Builtin ((_, name), _, _) -> "##" ^ name

        | Sort (_, Stype (SortLevel SLz)) -> "##Type"
        | Sort (_, Stype (SortLevel (SLsucc (SortLevel SLz)))) -> "##Type1"
        | Sort (_, Stype l) -> "(##Type_ " ^ lexp_string l ^ ")"
        | Sort (_, StypeLevel) -> "##Type_ℓ"
        | Sort (_, StypeOmega) -> "##Type_ω"

        | SortLevel (SLz) -> "##TypeLevel.z"
        | SortLevel (SLsucc e) -> "(##TypeLevel.succ " ^ lexp_string e ^ ")"
        | SortLevel (SLlub (e1, e2))
          -> "(##TypeLevel.∪ " ^ lexp_string e1 ^ " " ^ lexp_string e1 ^ ")"

and lexp_str_ctor ctx ctors =

  let pretty = pp_pretty ctx in
  let make_indent idt = if pretty then (make_line ' ' ((idt + (pp_indent ctx)) * (pp_size ctx))) else "" in
  let newline = (if pretty then "\n" else " ") in

  SMap.fold (fun key value str
             -> let str = str ^ newline ^ (make_indent 1) ^ "(" ^ key in
               let str = List.fold_left (fun str (k, _, arg)
                                         -> str ^ " " ^ (_lexp_str ctx arg))
                                        str value in
               str ^ ")")
            ctors ""

and _lexp_str_decls ctx decls =

    let lexp_str = _lexp_str ctx in
    let sepdecl = (if pp_decl ctx then "\n" else "") in

    let type_str name lxp = (if pp_type ctx then (
         name ^ " : " ^ (lexp_str lxp) ^ ";") else "") in

    let ret = List.fold_left
                (fun str ((_, name), lxp, ltp)
                 -> let str = if pp_type ctx then (type_str name ltp)::str else str in
                   (name ^ " = " ^ (lexp_str lxp) ^ ";" ^ sepdecl)::str)
                [] decls in
    List.rev ret

(** Syntactic equality (i.e. without β).  *******)

let rec eq meta_ctx e1 e2 =
  e1 == e2 ||
    let eq = eq meta_ctx in
    match (e1, e2) with
    | (Imm (Integer (_, i1)), Imm (Integer (_, i2))) -> i1 = i2
    | (Imm (Float (_, x1)), Imm (Float (_, x2))) -> x1 = x2
    | (Imm (String (_, s1)), Imm (String (_, s2))) -> s1 = s2
    | (Imm s1, Imm s2) -> s1 = s2
    | (SortLevel SLz, SortLevel SLz) -> true
    | (SortLevel (SLsucc e1), SortLevel (SLsucc e2)) -> eq e1 e2
    | (SortLevel (SLlub (e11, e21)), SortLevel (SLlub (e12, e22)))
      -> eq e11 e12 && eq e21 e22
    | (Sort (_, StypeOmega), Sort (_, StypeOmega)) -> true
    | (Sort (_, StypeLevel), Sort (_, StypeLevel)) -> true
    | (Sort (_, Stype e1), Sort (_, Stype e2)) -> eq e1 e2
    | (Builtin ((_, name1), _, _), Builtin ((_, name2), _, _)) -> name1 = name2
    | (Var (_, i1), Var (_, i2)) -> i1 = i2
    | (Susp (e1, s1), e2) -> eq (push_susp e1 s1) e2
    | (e1, Susp (e2, s2)) -> eq e1 (push_susp e2 s2)
    | (Let (_, defs1, e1), Let (_, defs2, e2))
      -> eq e1 e2 && List.for_all2 (fun (_, e1, t1) (_, e2, t2)
                                  -> eq t1 t2 && eq e1 e2)
                                 defs1 defs2
    | (Arrow (ak1, _, t11, _, t21), Arrow (ak2, _, t12, _, t22))
      -> ak1 = ak2 && eq t11 t12 && eq t21 t22
    | (Lambda (ak1, _, t1, e1), Lambda (ak2, _, t2, e2))
      -> ak1 = ak2 && eq t1 t2 && eq e1 e2
    | (Call (e1, as1), Call (e2, as2))
      -> eq e1 e2 && List.for_all2 (fun (ak1, e1) (ak2, e2) -> ak1 = ak2 && eq e1 e2)
                                 as1 as2
    | (Inductive (_, l1, as1, cases1), Inductive (_, l2, as2, cases2))
      -> l1 = l2
        && List.for_all2 (fun (ak1, _, e1) (ak2, _, e2) -> ak1 = ak2 && eq e1 e2)
                        as1 as2
        && SMap.equal (List.for_all2 (fun (ak1, _, e1) (ak2, _, e2)
                                     -> ak1 = ak2 && eq e1 e2))
                     cases1 cases2
    | (Cons (t1, (_, l1)), Cons (t2, (_, l2))) -> eq t1 t2 && l1 = l2
    | (Case (_, e1, r1, cases1, def1), Case (_, e2, r2, cases2, def2))
      -> eq e1 e2 && eq r1 r2
        && SMap.equal (fun (_, fields1, e1) (_, fields2, e2)
                      -> eq e1 e2 && List.for_all2 (fun (ak1, _) (ak2, _)
                                                  -> ak1 = ak2)
                                                 fields1 fields2)
                     cases1 cases2
        && (match (def1, def2) with
          | (Some (_, e1), Some (_, e2)) -> eq e1 e2
          | _ -> def1 = def2)
    | (Metavar (i1, s1, _, t1), Metavar (i2, s2, _, t2))
      -> i1 = i2 && eq t1 t2 && subst_eq meta_ctx s1 s2
    | _ -> false

and subst_eq meta_ctx s1 s2 =
  s1 == s2 ||
    match (s1, s2) with
    | (S.Identity, S.Identity) -> true
    | (S.Cons (e1, s1), S.Cons (e2, s2))
      -> eq meta_ctx e1 e2 && subst_eq meta_ctx s1 s2
    | (S.Shift (s1, o1), S.Shift (s2, o2))
      -> let o = min o1 o2 in
        subst_eq meta_ctx (S.mkShift s1 (o1 - o)) (S.mkShift s2 (o2 - o))
    | (S.Shift (S.Cons (e1, s1), o1), S.Cons (e2, s2))
      -> eq meta_ctx (mkSusp e1 (S.shift o1)) e2
        && subst_eq meta_ctx (S.mkShift s1 o1) s2
    | (S.Cons (e1, s1), S.Shift (S.Cons (e2, s2), o2))
      -> eq meta_ctx e1 (mkSusp e2 (S.shift o2))
        && subst_eq meta_ctx s1 (S.mkShift s2 o2)
    | _ -> false

