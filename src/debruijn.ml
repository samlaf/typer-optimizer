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
 *          Specifies recursive data structure for DeBruijn indexing
 *          methods starting with '_' are considered private and should not
 *          elsewhere in the project
 *
 * ---------------------------------------------------------------------------*)

open Util
open Lexp
open Myers
open Fmt

let debruijn_error = msg_error "DEBRUIJN"
let debruijn_warning = msg_warning "DEBRUIJN"

(*  Type definitions
 * ---------------------------------- *)

(*  Index -> Variable Info *)
type env_elem = (int * (location * string) * lexp option * ltype)
type env_type = env_elem myers

(* This exist because I don't want that file to depend on anything *)
module StringMap
    = Map.Make (struct type t = string let compare = String.compare end)

type db_idx  = int (* DeBruijn index.  *)
type db_ridx = int (* DeBruijn reverse index (i.e. counting from the root).  *)

(*  Map matching variable name and its distance in the current scope *)
type scope = db_ridx StringMap.t  (*  Map<String, db_ridx>*)

type senv_length = int  (* it is not the map true length *)
type senv_type = senv_length * scope

(* FIXME: What is this "outer_size" and "r_offset"?  *)
(* name -> index * index -> info * (outer_size, r_offset) *)
type lexp_context = senv_type * env_type * (int * int)

(*  internal definitions
 * ---------------------------------- *)

let _make_scope = StringMap.empty
let _make_senv_type = (0, _make_scope)
let _make_myers = nil
let _get_env(ctx: lexp_context): env_type = let (_, ev, _) = ctx in ev

(*  Public methods: DO USE
 * ---------------------------------- *)

let make_lexp_context = (_make_senv_type, _make_myers, (0, 0))

let get_roffset ctx = let (_, _, (_, rof)) = ctx in rof

(* FIXME: I have no idea what is a "scope" here!  *)

(* Enter a local scope such as let *)
let local_scope ctx pos =
    let ((l, map), b, c) = ctx in
    (*  Check size *)
    (* let n = (length b) in *)
    ((l, map), b, (l, pos))

(* Enter a temporary scope such as Calls *)
let temp_scope ctx bound = let (a, b, c) = ctx in (a, b, bound)

(*  return its current DeBruijn index *)
let rec senv_lookup (name: string) (ctx: lexp_context): int =
    let ((n, map), _, (csize, rof)) = ctx in
    let raw_idx =  n - (StringMap.find name map) - 1 in (*
        if raw_idx > (n - csize) then
            raw_idx - rof   (* Shift if the variable is not bound *)
        else *)
        raw_idx

(*  We first add variable into our map. Later on, we will add them into
 *  the environment. The reason for this is that the type info is
 *  known after lexp parsing which need the index fist *)
let senv_add_var (loc, name) ctx =
    let ((n, map), e, f) = ctx in
    (try let _ = senv_lookup name ctx in
         debruijn_warning loc ("Variable Shadowing " ^ name)
     with Not_found -> ());
    let nmap = StringMap.add name n map in
    ((n + 1, nmap), e, f)

let env_add_var_info var (ctx: lexp_context) =
    let (a, env, f) = ctx in
    (a, cons var env, f)

let env_extend (ctx:lexp_context) (def:vdef) (v: lexp option) (t:lexp) =
  env_add_var_info (0, def, v, t) (senv_add_var def ctx)

let env_lookup_type_by_index index ctx =
    try
        let (roffset, (_, name), _, t) = Myers.nth index (_get_env ctx) in
            Shift (index - roffset, t)
    with
        Not_found -> internal_error "DeBruijn index out of bounds!"

let env_lookup_type ctx (v : vref) =
  let ((_, rname), dbi) = v in
  try let (recursion_offset, (_, dname), _, t) = Myers.nth dbi (_get_env ctx) in
      if dname = rname then
        Shift (dbi - recursion_offset, t)
      else
        internal_error "DeBruijn index refers to wrong name!"
  with Not_found -> internal_error "DeBruijn index out of bounds!"

let env_lookup_by_index index (ctx: lexp_context): env_elem =
    Myers.nth index (_get_env ctx)
