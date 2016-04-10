(*
 *      Typer Compiler
 *
 * ---------------------------------------------------------------------------
 *
 *      Copyright (C) 2011-2016  Free Software Foundation, Inc.
 *
 *   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
 *   Keywords: languages, lisp, dependent types.
 *
 *   This file is part of Typer.
 *
 *   Typer is free software; you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any
 *   later version.
 *
 *   Typer is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *   more details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * ---------------------------------------------------------------------------
 *
 *      Description:
 *          Simple interpreter
 *
 * --------------------------------------------------------------------------- *)

open Util
open Fmt

open Sexp
open Pexp       (* Arg_kind *)
open Lexp

open Builtin
open Grammar

open Debruijn
open Env


let eval_error loc msg =
    msg_error "EVAL" loc msg;
    raise (internal_error msg)
;;

let dloc = dummy_location
let eval_warning = msg_warning "EVAL"

let _global_eval_trace = ref []
let _global_eval_ctx = ref make_runtime_ctx
let _eval_max_recursion_depth = ref 255
let reset_eval_trace () = _global_eval_trace := []

(*  currently, we don't do much *)
type value_type = lexp (*
    | Value of lexp
    | Closure of lexp * runtime_env *)

(* This is an internal definition
 * 'i' is the recursion depth used to print the call trace *)
let rec _eval lxp ctx i: (value_type) =
    let tloc = lexp_location lxp in

    (if i > (!_eval_max_recursion_depth) then
        raise (internal_error "Recursion Depth exceeded"));

    _global_eval_ctx := ctx; (*  Allow us to print the latest used environment *)
    _global_eval_trace := (i, tloc, lxp)::!_global_eval_trace;

    match lxp with
        (*  Leafs           *)
        (* ---------------- *)
        | Imm(v) -> lxp
        | Inductive (_, _, _, _) as e -> e
        | Cons (_, _) as e -> e

        (* Lambda's body is evaluated when called   *)
        | Lambda _ -> lxp (* Closure(lxp, ctx) *)

        (*  Return a value stored in the env        *)
        | Var((loc, name), idx) as e -> eval_var ctx e ((loc, name), idx)

        (*  Nodes           *)
        (* ---------------- *)
        | Let(_, decls, inst) ->
            let nctx = _eval_decls decls ctx i in
                _eval inst nctx (i + 1)

        (* Built-in Function * )
        | Call(Builtin(v, name, ltp), args) ->
            let nctx = build_arg_list args ctx i in *)

        (* Call to a builtin function *)
        | Call(b, args) when b = builtin_iadd ->
            let nctx = build_arg_list args ctx i in
                iadd_impl nctx

        | Call(b, args) when b = builtin_imult ->
            let nctx = build_arg_list args ctx i in
                imult_impl nctx

        (* Function call *)
        | Call (lname, args) -> eval_call ctx i lname args

        (* Case *)
        | Case (loc, target, _, pat, dflt) -> (eval_case ctx i loc target pat dflt)

        | _ -> Imm(String(dloc, "eval Not Implemented"))

and eval_var ctx lxp v =
    let ((loc, name), idx) = v in

    (* find variable binding i.e we do not want a another variable  *)
    (* (-3) represent a variable that should not be replaced        *)
    let rec var_crawling expr k =
        (if k > 255 then(
            lexp_print expr; print_string "\n"; flush stdout;
            eval_error loc "Variable lookup failed"));

        match expr with
            | Var(_, j) when j = (-3) -> lxp
            | Var((_, name2), j) ->
                var_crawling (get_rte_variable (Some name2) j ctx) (k + 1)
            | _ -> expr in

    try var_crawling lxp 0
    with Not_found ->
        eval_error loc ("Variable: " ^ name ^ (str_idx idx) ^ " was not found ")

and eval_call ctx i lname args =
    (* create a clean environment *)
    let tloc = lexp_location lname in
    let clean_ctx = temp_ctx ctx in
    let args_n = List.length args in

    (*  To handle partial call we need to consume args and   *)
    (*  lambdas together and return the remaining lambda     *)
    let rec consume_args (ctx: runtime_env) (lxp: lexp) args (k: int): value_type =
        match lxp, args with
            (* Base Case*)
            | Lambda(_, (_, name), _, body), arg::tl ->
                let ctx = add_rte_variable (Some name) arg ctx in
                    consume_args ctx body tl (k + 1)

            (* Partial Application *)
            (* In truth we don't really stop here. We push missing args *)
            (* as such that missing args will be replaced by themselves *)
            (* when the full eval branch will be called                 *)
            | Lambda(kind, (loc, name), l, body), [] ->
                let ctx = add_rte_variable (Some name) (Var((loc, name), -3)) ctx in
                let b = consume_args ctx body [] (k + 1) in
                (* Build a new lambda *)
                    Lambda(kind, (loc, name), l, b)

            (* Full Eval *)
            | _, [] ->
                _eval lxp ctx (k + i)

            (* Too many args *)
            | _, _ ->
                eval_error tloc ("Wrong Number of arguments." ^
                        " Got:" ^ (string_of_int args_n) ^ " arg(s)." ^
                        " Expected: " ^ (string_of_int k) ^ " arg(s)") in

    match lname with
        (* This is a named function call *)
        | Var((_, name), idx) -> (
            (*  get function's body from current context *)
            let body = get_rte_variable (Some name) idx ctx in

            (*  _eval every args using current context *)
            let arg_val = List.map (fun (k, e) -> _eval e ctx (i + 1)) args in

            match body with
                (* If 'cons', build it back with evaluated args *)
                | Cons _ ->
                    Call(lname, (List.map (fun g -> (Aexplicit, g)) arg_val))

                | Lambda _ ->
                    (consume_args clean_ctx body arg_val 0)

                | _ ->
                (*  Add args inside our clean context *)
                let nctx = List.fold_left (fun c v -> add_rte_variable None v c)
                    clean_ctx arg_val in
                    _eval body nctx (i + 1))

        (* I am not sure something could be there           *)
        (* Not sure if this is legal:                       *)
        (*  ((lambda x -> lambda y -> x + y) 2 3)           *)
        | _ -> Imm(String(dloc, "Funct Not Implemented"))

and eval_case ctx i loc target pat dflt =
    (* Eval target *)
    let v = _eval target ctx (i + 1) in

    let (_, (osize, _)) = ctx in    (* number of variable declared outside *)
    let csize = get_rte_size ctx in (* current size                        *)
    let offset = csize - osize in

    (* Get inductive type from a constructor *)
    let get_inductive_ref lxp =
        match lxp with
            | Cons(((_, vname), idx), (_, cname)) ->(
                let info = get_rte_variable (Some vname) (idx + offset) ctx in
                let ctor_def = match info with
                    | Inductive(_, _, _, c) -> c
                    | _ -> eval_error loc "Not an Inductive Type" in
                    info, ctor_def)
            | _ -> eval_error loc "Target is not a Constructor" in

    (* I think checks should be in lexp not done during eval *)
    (* check if a constructor 'cname' exist                  *)
    let check_ctor cname cargs ctor_def =
        try let targs = (SMap.find cname ctor_def) in
            let n_targs = List.length targs in
            let n_cargs = List.length cargs in

            match (n_targs - n_cargs) with
                | 0 -> cname, cargs
                | _ -> eval_error loc ("Constructor not applied. " ^
                    cname ^ " expected " ^ (string_of_int n_targs) ^ " arg(s)." ^
                    " Given " ^ (string_of_int n_cargs) ^ " arg(s).")

        with Not_found ->
            eval_error loc ("Constructor \"" ^ cname ^ "\" does not exist") in

    (* extract constructor name and check its validity *)
    let ctor_name, args = match v with
        | Call(Var((_, cname), tp), args) ->(
            (* Don't check the constructor this job will be done in lexping *)
                cname, args)

            (* get constructor * )
            try let ctor = get_rte_variable None (tp + offset) ctx in
                let idt, ctor_def = get_inductive_ref ctor in
                    check_ctor cname args ctor_def

            (* currently we have a little bug with ctor checking *)
            with e -> cname, args) *)

        | Cons(((_, vname), idx), (_, cname)) as ctor ->
            let idt, ctor_def = get_inductive_ref ctor in
                check_ctor cname [] ctor_def

        | _ -> lexp_print target; print_string "\n";
            lexp_print v; print_string "\n";
            eval_error loc "Target is not a Constructor" in

    (*  Check if a default is present *)
    let run_default df =
        match df with
        | None -> eval_error loc "Match Failure"
        | Some lxp -> _eval lxp ctx (i + 1) in

    let ctor_n = List.length args in

    (*  Build a filter option *)
    let is_true key value =
        let (_, pat_args, _) = value in
        let pat_n = List.length pat_args in
            (* FIXME: Shouldn't pat_n != ctor_n cause an error ? *)
            if pat_n = ctor_n && ctor_name = key then
                true
            else
                false in

    (* if the argument is a reference to a variable its index need to be
     * shifted  *)
    let arg_shift xp offset =
        match xp with
            | Var(a, idx) -> Var(a, idx + offset)
            | _ -> xp in

    (*  Search for the working pattern *)
    let sol = SMap.filter is_true pat in
        if SMap.is_empty sol then
            run_default dflt
        else
            (*  Get working pattern *)
            let key, (_, pat_args, exp) = SMap.min_binding sol in

            (* count the number of declared variables *)
            let case_offset = List.fold_left (fun i g ->
                match g with None -> i | _ -> i + 1)
                0 pat_args in

            let toffset = case_offset + offset in

            (* build context *)
            let nctx = List.fold_left2 (fun nctx pat arg ->
                match pat with
                    | None -> nctx
                    | Some (_, (_, name)) ->
                        let (_, xp) = arg in
                        let xp = (arg_shift xp toffset) in
                            add_rte_variable (Some name) xp nctx)

                ctx pat_args args in
                    (* eval body *)
                    _eval exp nctx (i + 1)

and build_arg_list args ctx i =
    (*  _eval every args *)
    let arg_val = List.map (fun (k, e) -> _eval e ctx (i + 1)) args in

    (*  Add args inside context *)
    List.fold_left (fun c v -> add_rte_variable None v c) ctx arg_val

and eval_decls decls ctx = _eval_decls decls ctx 1
and _eval_decls (decls: ((vdef * lexp * ltype) list))
               (ctx: runtime_env) i: runtime_env =

    (* Read declarations once and push them *)
    let ctx = List.fold_left (fun ctx ((_, name), lxp, ltp) ->
        add_rte_variable (Some name) lxp ctx)
        ctx decls in

    (* local ctx saves the number of declared variable inside ctx      *)
    (* This is used to remove temporary variables when entering a Call *)
    let ctx = local_ctx ctx in
    let n = (List.length decls) - 1 in

    (* Read declarations once and push them *)
    let _, ctx = List.fold_left (fun (idx, ctx) ((_, name), lxp, ltp) ->
        let lxp = _eval lxp ctx (i + 1) in
        let ctx = set_rte_variable idx (Some name) lxp ctx in
        (idx - 1, ctx))
        (n, ctx) decls in

        ctx

and print_eval_result i lxp =
    print_string "     Out[";
    ralign_print_int i 2;
    print_string "] >> ";
    match lxp with
        | Imm(v) -> sexp_print v; print_string "\n"
        | e ->  lexp_print e; print_string "\n"

and print_eval_trace () =
    print_trace " EVAL TRACE " 50 lexp_to_string lexp_print !_global_eval_trace
;;

let eval lxp ctx =
    _global_eval_trace := [];
    _eval lxp ctx 1

let debug_eval lxp ctx =
    try
        eval lxp ctx
    with e -> (
        print_rte_ctx (!_global_eval_ctx);
        print_eval_trace ();
        raise e)
;;

(*  Eval a list of lexp *)
let eval_all lxps rctx silent =
    let evalfun = if silent then eval else debug_eval in
    List.map (fun g -> evalfun g rctx) lxps;;


