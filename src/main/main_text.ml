(******************************************************************************)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

open Why_ptree
open Frontend

open Lexing
open Format
open Options

let _ = 
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ -> print_endline "User wants me to stop."; exit 1))	  


let _ =
  Time.set_timeout ();
  Options.parse_args ();
  let cin = get_in_channel () in
  let lb = from_channel cin in 
  try 
    let d, status = open_file lb cin in 
    processing print_status d;
    Time.unset_timeout ();
  with
    | Why_lexer.Lexical_error s -> 
      Loc.report err_formatter (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
    | Parsing.Parse_error ->
      let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
      Loc.report err_formatter loc;
      eprintf "syntax error\n@.";
      exit 1
    | Errors.Error(e,l) -> 
      Loc.report err_formatter l; 
      eprintf "typing error: %a\n@." Errors.report e;
      exit 1

