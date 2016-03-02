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
 *          print out each compilation' steps
 *
 * --------------------------------------------------------------------------- *)

open Debug
open Prelexer
open Lexer
open Sexp
open Grammar
open Pexp
open Debruijn
open Lparse
open Myers
open Eval

(*  pexp and lexp can be done together, one by one *)
let pexp_lexp_one node ctx = 
    let pxp = pexp_parse node in
    lexp_parse pxp ctx
;;

let pexp_lexp_all nodes ctx =

    let rec loop nodes ctx acc = 
        match nodes with
            | [] -> ((List.rev acc), ctx)
            | hd::tl  -> 
                let lxp, new_ctx = pexp_lexp_one hd ctx in
                    (loop tl new_ctx (lxp::acc)) in
    (loop nodes ctx [])
;;
        
let main () = 

    let arg_n = Array.length Sys.argv in
    let usage = 
        "  Usage: \n    " ^ Sys.argv.(0) ^ " <file_name> [options] \n\n" in
    
    let print_title msg = 
        print_string "\n\t====\n";
        print_string ("\t    " ^ msg ^ "\n");
        print_string "\t=======================\n" in
    
    (*  Print Usage *)
    if arg_n == 1 then
        begin
        print_string usage;
        print_string "  Options:\n";
        end
    else
    begin
        let filename = Sys.argv.(1) in
        
        (* Read additional Args if any *)

        (* get pretokens*)
        let pretoks = prelex_file filename in
        
        (* get sexp/tokens *)
        let toks = lex default_stt pretoks in
        
        (* get node sexp  *)
        let nodes = sexp_parse_all_to_list default_grammar toks (Some ";") in
        
        (* get pexp *)
        let pexps = pexp_parse_all nodes in

        (* get lexp *)
        let ctx = make_context in
        let lexps, new_ctx = lexp_parse_all pexps ctx in
        
        (* Printing *)(*
        print_title "PreTokens";    debug_pretokens_print_all pretoks;
        print_title "Base Sexp";    debug_sexp_print_all toks;  *)
        print_title "Node Sexp";    debug_sexp_print_all nodes;
        print_title "Pexp";         debug_pexp_print_all pexps;
        print_title "Lexp";         debug_lexp_print_all lexps;
        
        (* Eval Each Expression *)
        print_title "Eval Print";
        
        (*  Eval One *)
        let rctx = make_runtime_ctx in
        let c, rctx = eval (List.hd lexps) rctx in
            print_eval_result c
    
    end
;;

main ()
;;

