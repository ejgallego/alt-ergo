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
(*     Claire Dross                                                       *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

let fmt = Format.err_formatter

module M = struct

  let file = ref " stdin"
  let session_file = ref ""
  let cin = ref stdin
  let rewriting = ref false
  let type_only = ref false
  let parse_only = ref false
  let stop_bound = ref 8
  let steps_bound = ref (-1)
  let age_bound = ref 10
  let debug = ref false
  let notriggers = ref false
  let debug_cc = ref false
  let debug_use = ref false
  let debug_arrays = ref false
  let debug_uf = ref false
  let debug_sat = ref false
  let debug_sat_simple = ref false
  let debug_typing = ref false
  let debug_constr = ref false
  let verbose = ref false
  let debug_fm = ref false
  let debug_sum = ref false
  let debug_arith = ref false
  let debug_combine = ref false
  let debug_bitv = ref false
  let debug_ac = ref false
  let debug_split = ref false
  let options = ref false
  let smtfile = ref false
  let smt2file = ref false
  let satmode = ref false
  let glouton = ref false
  let triggers_var = ref false
  let nb_triggers = ref 2
  let inversion_axioms = ref false
  let enable_assertions = ref false
  let select = ref 0
  let no_rm_eq_existential = ref false
  let nocontracongru = ref false
  let term_like_pp = ref true
  let debug_types = ref false 
  let all_models = ref false
  let model = ref false
  let complete_model = ref false
  let goal_directed = ref false
  let proof = ref false
  let debug_proof = ref false
  let rules = ref (-1)
  let max_split = ref (Numbers.Q.of_int 1000000)
  let restricted = ref false
  let bottom_classes = ref false
  let timelimit = ref 0.
  let debug_matching = ref false

  let show_version () = Format.printf "%s@." Version.version; exit 0
  let show_libdir () = Format.printf "%s@." Version.libdir; exit 0

  let set_max_split s = 
    max_split := 
      try Numbers.Q.of_string s 
      with Failure _ -> Numbers.Q.m_one

  let set_proof b = proof := b

  let set_rules = function
    | "parsing" -> rules := 0
    | "typing" -> rules := 1
    | "sat" -> rules := 2
    | "cc" -> rules := 3
    | "arith" -> rules := 4
    | _ -> rules := -1

  let set_limit t =
    match Sys.os_type with
      | "Win32" -> Format.eprintf "timelimit not supported on Win32 (ignored)@."
      | _ -> timelimit := t

  let replay = ref false

  let debug_custom = ref false

  let usage = "usage: alt-ergo [options] file.<mlw|smt>"

  let spec = [
    "-inversion-axioms", Arg.Set inversion_axioms, "  instantiate inversion axioms before the others";
    "-rwt", Arg.Set rewriting, " use rewriting instead of axiomatic approach";
    "-parse-only", Arg.Set parse_only, " stop after parsing";
    "-type-only", Arg.Set type_only , " stop after typing";
    "-notriggers", Arg.Set notriggers, "  trigger inference";
    "-debug", Arg.Set debug, "  sets the debugging flag";
    "-dcc", Arg.Set debug_cc, "  sets the debugging flag of cc";
    "-duse", Arg.Set debug_use, "  sets the debugging flag of use";
    "-duf", Arg.Set debug_uf, "  sets the debugging flag of uf";
    "-dfm", Arg.Set debug_fm, "  sets the debugging flag of inequalities";
    "-dsum", Arg.Set debug_sum, "  sets the debugging flag of Sum";
    "-darith", Arg.Set debug_arith, 
    " sets the debugging flag of Arith (without fm)";
    "-dbitv", Arg.Set debug_bitv, "  sets the debugging flag of bitv";
    "-dac", Arg.Set debug_ac, "  sets the debugging flag of ac";
    "-dsat", Arg.Set debug_sat, "  sets the debugging flag of sat";
    "-dsats", Arg.Set debug_sat_simple, 
    "  sets the debugging flag of sat (simple output)";
    "-dtyping", Arg.Set debug_typing, "  sets the debugging flag of typing";
    "-types", Arg.Set debug_types, "  sets the debugging flag of types";
    "-dconstr", Arg.Set debug_constr, 
    "  sets the debugging flag of constructors";
    "-darrays", Arg.Set debug_arrays, "  sets the debugging flag of arrays";
    "-dcombine", Arg.Set debug_combine, "  sets the debugging flag of combine";
    "-dsplit", Arg.Set debug_split, "  sets the debugging flag of case-split analysis";
    "-dtheory", Arg.Set debug_custom, "  sets the debugging flag of user-defined theories";
    "-dmatching", Arg.Set debug_matching, "  sets the debugging flag of E-matching";
    "-v", Arg.Set verbose, "  sets the verbose mode";
    "-version", Arg.Unit show_version, "  prints the version number";
    "-where", Arg.Unit show_libdir, "  prints the directory of the library";
    "-stop-bound", Arg.Set_int stop_bound, " <n> set the stop bound for the SAT";
    "-steps-bound", Arg.Set_int steps_bound, " <n> set the maximum number of steps";
    "-age-bound", Arg.Set_int age_bound, " <n> set the age limite bound";
    "-sat-mode" , Arg.Set satmode , " mode sat/unsat";
    "-glouton" , Arg.Set glouton, 
    " use ground terms in non-instanciated lemmas";
    "-nb-triggers" , Arg.Set_int nb_triggers, 
    " number of redondant (multi)triggers (default: 2)";
    "-select" , Arg.Set_int select, 
    "k tries to select relevant (at level k) hypotheses for each goal";
    "-triggers-var" , Arg.Set triggers_var , " allows variables as triggers";
    "-no-rm-eq-existential", Arg.Set no_rm_eq_existential, " does not substitute a variable in an existential when an equality gives the value of the variable";
    "-nocontracongru", Arg.Set nocontracongru, "";
    "-term-like-pp", Arg.Set term_like_pp, "Output semantic values as terms";
    "-all-models", Arg.Set all_models, "experimental support for all models";
    "-model", Arg.Set model, "experimental support for models on labeled terms";
    "-complete-model", Arg.Set complete_model, "experimental support for complete model";
    "-proof", Arg.Set proof, "experimental support for succint proof";
    "-debug-proof", Arg.Set debug_proof, "experimental support for succint proof";
    "-goal-directed", Arg.Set goal_directed,
    " instantiate lemmas only with the terms from the goal";
    "-rules", Arg.String set_rules, "<parsing|typing|sat|cc|arith> output rules used on stderr";
    "-max-split", Arg.String set_max_split,
    (Format.sprintf " maximum size of case-split (default value : %s)" 
       (Numbers.Q.string_of !max_split));
    "-restricted", Arg.Set restricted, 
    " restrict set of decision procedures (equality, arithmetic and AC)";
    "-bottom-classes", Arg.Set bottom_classes, "show equivalence classes at each bottom of the sat";
    "-replay", Arg.Set replay, "replay session saved in .agr";
    "-timelimit", Arg.Float set_limit, "n set the time limit to n seconds (not supported on Windows)";
  ]

  let profiling = ref false
  let thread_yield = ref (fun () -> ())
  let (timer_start : (Timers.kind -> unit) ref) = ref (fun _ -> ())
  let (timer_pause : (Timers.kind -> unit) ref) = ref (fun _ -> ())
  let (timeout : (unit -> unit) ref) =
    ref (fun () -> Format.printf "Timeout@."; exit 142)

end

let parse_args () =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".mlw" || Filename.check_suffix s ".why"
    then ofile := Some s
    else
      if Filename.check_suffix s ".smt"
      then begin 
	M.smtfile := true ;
        ofile := Some s
      end
      else
	if Filename.check_suffix s ".smt2"
	then begin 
	  M.smt2file := true; 
          ofile := Some s
	end
	else raise (Arg.Bad "no .mlw, .smt or smt2 extension")
  in
  Arg.parse M.spec set_file M.usage;
  match !ofile with 
    | Some f -> 
      M.file := f ; 
      M.cin := open_in f;
      M.session_file := (Filename.chop_extension f)^".agr"
    | None ->
      M.smt2file := true; 
      M.cin := stdin


(** setter functions **********************************************************)

(** setters for debug flags *)
let set_debug b = M.debug := b
let set_debug_cc b = M.debug_cc := b
let set_debug_use b = M.debug_use := b
let set_debug_uf b = M.debug_uf := b
let set_debug_fm b = M.debug_fm := b
let set_debug_sum b = M.debug_sum := b
let set_debug_arith b = M.debug_arith := b
let set_debug_bitv b = M.debug_bitv := b
let set_debug_ac   b = M.debug_ac := b
let set_debug_sat b = M.debug_sat := b
let set_debug_sat_simple b = M.debug_sat_simple := b
let set_debug_typing b = M.debug_typing := b
let set_debug_constr b = M.debug_constr := b
let set_debug_arrays b = M.debug_arrays := b
let set_debug_types b = M.debug_types := b
let set_debug_combine b = M.debug_combine := b
let set_debug_proof b = M.debug_proof := b
let set_debug_split b = M.debug_split := b
let set_debug_custom b = M.debug_custom := b
let set_debug_matching b = M.debug_matching := b

(** additional setters *)
let set_type_only b = M.type_only := b
let set_parse_only b = M.parse_only := b
let set_stop_bound b = M.stop_bound := b
let set_steps_bound b = M.steps_bound := b
let set_age_bound b = M.age_bound := b
let set_notriggers b = M.notriggers := b
let set_verbose b = M.verbose := b
let set_glouton b = M.glouton := b
let set_triggers_var b = M.triggers_var := b
let set_nb_triggers b = M.nb_triggers := b
let set_select b = M.select := b
let set_no_rm_eq_existential b = M.no_rm_eq_existential := b
let set_nocontracongru b = M.nocontracongru := b
let set_term_like_pp b = M.term_like_pp := b
let set_all_models b = M.all_models := b
let set_model b = M.model := b
let set_complete_model b = M.complete_model := b
let set_goal_directed b = M.goal_directed := b
let set_max_split b = M.max_split := b
let set_rewriting b = M.rewriting := b
let set_proof b = M.proof := b
let set_rules b = M.rules := b
let set_restricted b = M.restricted := b
let set_bottom_classes b = M.bottom_classes := b
let set_timelimit b = M.timelimit := b
let set_profiling b = M.profiling := b
let set_thread_yield f = M.thread_yield := f
let set_timer_start f = M.timer_start := f
let set_timer_pause f = M.timer_pause := f
let set_timeout f = M.timeout := f


(** getter functions **********************************************************)

(** getters for debug flags *)
let debug () = !M.debug
let debug_cc () = !M.debug_cc
let debug_use () = !M.debug_use
let debug_uf () = !M.debug_uf
let debug_fm () = !M.debug_fm
let debug_sum () = !M.debug_sum
let debug_arith () = !M.debug_arith
let debug_bitv () = !M.debug_bitv
let debug_ac   () = !M.debug_ac
let debug_sat () = !M.debug_sat
let debug_sat_simple () = !M.debug_sat_simple
let debug_typing () = !M.debug_typing
let debug_constr () = !M.debug_constr
let debug_custom () = !M.debug_custom
let debug_arrays () = !M.debug_arrays
let debug_types () = !M.debug_types
let debug_combine () = !M.debug_combine
let debug_proof () = !M.debug_proof && !M.proof
let debug_split () = !M.debug_split
let debug_matching () = !M.debug_matching

(** additional getters *)
let type_only () = !M.type_only
let parse_only () = !M.parse_only
let stop_bound () = !M.stop_bound
let steps_bound () = !M.steps_bound
let age_bound () = !M.age_bound
let notriggers () = !M.notriggers
let verbose () = !M.verbose
let glouton () = !M.glouton
let triggers_var () = !M.triggers_var
let nb_triggers () = !M.nb_triggers
let select () = !M.select
let no_rm_eq_existential () = !M.no_rm_eq_existential
let nocontracongru () = !M.nocontracongru
let term_like_pp () = !M.term_like_pp
let all_models () = !M.all_models
let model () = !M.model || !M.complete_model
let complete_model () = !M.complete_model
let goal_directed () = !M.goal_directed
let max_split () = !M.max_split
let rewriting () = !M.rewriting
let proof () = !M.proof
let rules () = !M.rules
let restricted () = !M.restricted
let bottom_classes () = !M.bottom_classes
let timelimit () = !M.timelimit
let inversion_axioms () = !M.inversion_axioms
let enable_assertions () = !M.enable_assertions
let profiling () = !M.profiling

let get_in_channel () = !M.cin
let replay () = !M.replay
let get_file () = !M.file
let get_session_file () = !M.session_file
let satmode () = !M.satmode
let smt2file () = !M.smt2file
let smtfile () = !M.smtfile


(** particular getters : functions that are immediately executed **************)
let exec_thread_yield () = !M.thread_yield ()
let exec_timer_start kd = !M.timer_start kd
let exec_timer_pause kd = !M.timer_pause kd
let exec_timeout () = !M.timeout ()

let tool_req n msg = 
  if rules () = n then Format.fprintf fmt "[rule] %s@." msg
