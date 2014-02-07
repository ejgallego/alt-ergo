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
(*     Claire Dross                                                           *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

open Why_ptree

let _ = 
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ -> print_endline "User wants me to stop."; exit 1))

open Lexing
open Format
open Options

module Time = struct

  open Unix
    
  let u = ref 0.0
    
  let start () = u:=(times()).tms_utime

  let get () = 
    let res = (times()).tms_utime -. !u in
    start();
    res


  let set_timeout () =
    if timelimit () <> 0. then
      ignore (Unix.setitimer Unix.ITIMER_REAL
		{ Unix.it_value = timelimit (); Unix.it_interval = 0. })
	
  let unset_timeout () =
    if timelimit () <> 0. then
      ignore (Unix.setitimer Unix.ITIMER_REAL
		{ Unix.it_value = 0.; Unix.it_interval = 0. })

end

type output = Unsat of Explanation.t | Inconsistent 
	      | Sat of Sat.t | Unknown of Sat.t

let check_produced_proof dep =
  if verbose () then 
    fprintf fmt "checking the proof:\n-------------------\n%a@." 
      Explanation.print_proof dep;

  try
    let _ = 
      (Formula.Set.fold
         (fun f env -> 
           Sat.assume env 
	     {Sat.f=f;age=0;name=None;mf=false;
	      gf=false; from_terms = []; inv=false}
         ) (Explanation.formulas_of dep) (Sat.empty ()))
    in
    fprintf fmt "Checking produced proof failed!@.";
    fprintf fmt "this may be due to a bug.@.";
    exit 1
  with 
    | Sat.Unsat _  -> ()
    | (Sat.Sat _ | Sat.I_dont_know _) as e -> raise e


let process_decl print_status (env, consistent, dep) d =
  try
    match d.st_decl with
      | Assume(f, mf, inv) -> 
	Sat.assume env 
	  {Sat.f=f; age=0; name=None; 
	   mf=mf; gf=false; from_terms = []; inv=inv},
	consistent, dep

      |	PredDef f -> 
	Sat.pred_def env f , consistent, dep

      | RwtDef r -> assert false

      | Query(n, f, lits, sort) ->
	let dep = 
	  if consistent then
	    let dep' = Sat.unsat env 
	      {Sat.f=f;age=0;name=None;
	       mf=(sort <> Check);gf=true; from_terms = []; inv=false} in
	    Explanation.union dep' dep
	  else dep
        in
        if debug_proof () then check_produced_proof dep;
	print_status d (Unsat dep) (Sat.stop ());
	env, consistent, dep
  with 
    | Sat.Sat t -> 
      print_status d (Sat t) (Sat.stop ());
      if model () then Sat.print_model ~header:true std_formatter t;
      env , consistent, dep
    | Sat.Unsat dep' -> 
      let dep = Explanation.union dep dep' in
      if debug_proof () then check_produced_proof dep;
      print_status d Inconsistent (Sat.stop ());
      env , false, dep
    | Sat.I_dont_know t -> 
      print_status d (Unknown t) (Sat.stop ());
      if model () then Sat.print_model ~header:true std_formatter t;
      env , consistent, dep

exception Parse_only

let rec add_theory env s =
  let c_in = open_in s in
  let lb = from_channel c_in in 
  try 
    let includes, a = Why_parser.file Why_lexer.token lb in
    let env = List.fold_left add_theory env includes in
    if parse_only () then raise Parse_only;
    let d, env = Why_typing.file true env a in
    close_in c_in;
    let d = List.map (fun (d, _) -> (d, true)) d in
    let cnf =  Cnf.make_theory d in
    let f = Queue.fold (fun formulas d ->
      match d.st_decl with
        | Assume (f, _, _ ) | PredDef f ->
          f :: formulas
        | RwtDef _ | Query _ -> assert false)
      [] cnf in
    Custom_theory.add_theory f;
    env
  with
    | Parse_only -> env
    | Why_lexer.Lexical_error s -> 
      Loc.report err_formatter (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error in theory: %s\n@." s;
      exit 1
    | Parsing.Parse_error ->
      let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
      Loc.report err_formatter loc;
      eprintf "syntax error in theory\n@.";
      exit 1
    | Errors.Error(e,l) -> 
      Loc.report err_formatter l; 
      eprintf "typing error in theory: %a\n@." Errors.report e;
      exit 1


let open_file lb cin =
  let file = Options.get_file() in
  let d ,status =
    if smtfile() then begin
      let bname,l,status = Smt_parser.benchmark Smt_lex.token lb in
      if verbose () then printf "converting smt file : ";
      let l = List.flatten (List.map Smt_to_why.bench_to_why l) in
      if verbose () then printf "done.@.";
      if parse_only () then exit 0;
      let ltd, _ = Why_typing.file false Why_typing.empty_env l in
      let lltd = Why_typing.split_goals ltd in
      lltd, status
    end
    else if smt2file() then begin
      let commands = Smtlib2_parse.main Smtlib2_lex.token lb in
      if verbose () then printf "converting smt2 file : ";
      let l = Smtlib2_to_why.smt2_to_why commands in
      if verbose () then printf "done.@.";
      if parse_only () then exit 0;
      let ltd, _ = Why_typing.file false Why_typing.empty_env l in
      let lltd = Why_typing.split_goals ltd in
      lltd, Smt_ast.Unknown
    end
    else
      let includes, a = Why_parser.file Why_lexer.token lb in
      (* Customized theories given as include *)
      let env = List.fold_left add_theory Why_typing.empty_env includes in
      if parse_only () then exit 0;
      let ltd, _ = Why_typing.file false env a in
      let lltd = Why_typing.split_goals ltd in
      lltd, Smt_ast.Unknown
  in
  if file <> " stdin" then close_in cin;
  if type_only () then exit 0;
  d, status

let pruning = 
  List.map
    (fun d -> 
      if select () > 0 then Pruning.split_and_prune (select ()) d 
      else [List.map (fun f -> f,true) d])
    
let processing report declss = 
  Sat.start ();
  let declss = List.map (List.map fst) declss in
  List.iter
    (List.iter 
       (fun dcl ->
	 let cnf = Cnf.make dcl in 
	 ignore (Queue.fold (process_decl report)
		   (Sat.empty (), true, Explanation.empty) cnf)
       )) (pruning declss)


let print_status d s steps =
  let satmode = smtfile() || smt2file() || satmode() in
  match s with
    | Unsat dep -> 
      if not satmode then Loc.report std_formatter d.st_loc;
      if satmode then printf "@{<C.F_Red>unsat@}@." 
      else printf "@{<C.F_Green>Valid@} (%2.4f) (%Ld)@." (Time.get()) steps;
      if proof () && not (debug_proof ()) then 
        printf "Proof:\n%a@." Explanation.print_proof dep
	  
    | Inconsistent ->
      if not satmode then 
	(Loc.report std_formatter d.st_loc; 
	 fprintf fmt "Inconsistent assumption@.")
      else printf "unsat@."
	
    | Unknown t ->
      if not satmode then
	(Loc.report std_formatter d.st_loc; printf "I don't know.@.")
      else printf "unknown@."
	
    | Sat t -> 
      if not satmode then Loc.report std_formatter d.st_loc;
      if satmode then printf "unknown (sat)@." 
      else printf "I don't know@."