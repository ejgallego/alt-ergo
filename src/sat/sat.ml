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

open Options
open Format
open Sig

module A = Literal
module CcX = Cc.Make(Combine.CX)
module F = Formula
module MM = Matching.Make(CcX) 
module SF = F.Set
module MF = F.Map
module Ex = Explanation

let steps = ref 0L

type gformula = { 
  f: F.t; 
  age: int; 
  name: F.t option; 
  from_terms : Term.t list;
  mf: bool;
  gf: bool;
  inv : bool;
}

module H = Hashtbl.Make(Formula)

type t = { 
  gamma : Ex.t MF.t; 
  delta : (gformula * gformula * Ex.t) list;
  tbox : CcX.t;
  lemmas : (int * Ex.t) MF.t;
  definitions : (int * Ex.t) MF.t;
  inversions :  (int * Ex.t) MF.t;
  matching : MM.t;
  add_inst: Formula.t -> unit;
}

exception Sat of t
exception Unsat of Ex.t
exception I_dont_know of t
exception IUnsat of Ex.t * Term.Set.t list

(*
  a huge limit <~~> no limit,
  previous value is 96, but it is not suitable for BWare VCs 
*)
let max_max_size = max_int 

(*BISECT-IGNORE-BEGIN*)
module Debug = struct

  let unsat_rec dep =
    if debug_sat () then fprintf fmt "unsat_rec : %a@." Ex.print dep

  let assume {f=f;age=age;name=lem;mf=mf;from_terms=terms} dep = 
    if debug_sat () then
      begin
	(match F.view f with
	  | F.Unit _ -> ()
	    
	  | F.Clause _ -> 
	    fprintf fmt "[sat] I assume a clause %a@." F.print f
	      
	  | F.Lemma _ ->
	    fprintf fmt "[sat] I assume a [%d-atom] lemma: %a@."
              (F.size f) F.print f
	      
	  | F.Literal a -> 
	    Term.print_list str_formatter terms;
	    let s = flush_str_formatter () in
	    let n = match lem with 
	      | None -> ""  
	      | Some ff -> 
		(match F.view ff with F.Lemma xx -> xx.F.name | _ -> "")
	    in
	    fprintf fmt "\n[sat]I assume a literal (%s : %s) %a@]@."
              n s Literal.LT.print a;
	    fprintf fmt "================================================@.@."
	      
	  | F.Skolem{F.sko_subst=(s,s_ty); sko_f=f} ->
	    fprintf fmt "[sat] I assume a skolem %a@." F.print f 
	      
	  | F.Let {F.let_var=lvar; let_term=lterm; let_f=lf} ->
	    fprintf fmt "[sat] I assume a let %a = %a in %a@." 
	      Symbols.print lvar Term.print lterm F.print lf);
	fprintf fmt " with explanations : %a@." Explanation.print dep
      end

  let unsat () = 
    if debug_sat () then fprintf fmt "[sat] unsat@."

  let mround s = 
    if debug_sat () then 
      fprintf fmt "[sat] matching round of size %d@." s

  let decide f = 
    if debug_sat () then 
      fprintf fmt "[sat] I decide on %a@." F.print f

  let backtracking f = 
    if debug_sat () then 
      fprintf fmt "[sat] I backtrack on %a@." F.print f

  let backjumping f = 
    if debug_sat () then 
      fprintf fmt "[sat] I don't consider the case %a@." F.print f
        
  let elim _ _ = 
    if debug_sat () && verbose () then fprintf fmt "[sat] elim@."

  let red _ _ = 
    if debug_sat () && verbose () then fprintf fmt "[sat] red@."

  let delta d = 
    if debug_sat () && verbose () then begin
      fprintf fmt "[sat] - Delta ---------------------@.";
      List.iter (fun (f1, f2, ex) ->
	fprintf fmt "(%a or %a), %a@." F.print f1.f F.print f2.f Ex.print ex) d;
      fprintf fmt "[sat] --------------------- Delta -@."
    end
      
  let gamma g =
    if debug_sat () then begin
      fprintf fmt "[sat] - GAMMA ---------------------@.";
      MF.iter (fun f ex ->
	fprintf fmt  "%a \t->\t%a@." F.print f Ex.print ex) g;
      fprintf fmt  "[sat] --------------------- GAMMA -@."
    end
      
  let bottom classes =
    if bottom_classes () then
      printf "bottom:%a\n@." Term.print_tagged_classes classes      
        
  let inconsistent expl =
    if debug_sat () then fprintf fmt "inconsistent %a@." Ex.print expl

end
(*BISECT-IGNORE-END*)

(* matching part of the solver *)

let add_terms env s goal age lem terms =
  Options.exec_timer_start Timers.TMatch;
  let infos = { 
    Matching.term_age = age ; 
    term_from_goal = goal ;
    term_from_formula = lem ;
    term_from_terms = terms
  }
  in
  let env = 
    { env with matching = Term.Set.fold (MM.add_term infos) s env.matching } in
  Options.exec_timer_pause Timers.TMatch;
  env

exception EnoughLemmasAlready of t * int

let b_max_size = 100

let rec double_until min s =
  let s2 = s + b_max_size in 
  if s2 >= min then s2 else double_until min s2

let mtriggers env formulas max_size = 
  let stop = ref false in
  try
    MF.fold
      (fun lem (age, dep) (env, max_size) ->
	let size = F.size lem in
	let max_size = 
	  if size <= max_size then max_size 
	  else 
	    begin
	      if !stop then raise (EnoughLemmasAlready(env, max_size));
	      stop:=true; double_until size max_size
	    end
	in
	let env = 
	  match F.view lem with
	      F.Lemma {F.triggers = tgs; main = f} -> 
		List.fold_left 
		  (fun env (tg, a) ->
		    let info = 
		      { Matching.trigger_age = age ; 
			trigger_orig = lem ;
			trigger_formula = f ;
			trigger_dep = dep ; 
			trigger_query = a }
		    in
		    { env with 
		      matching = 
			MM.add_trigger info tg env.matching })
		  env tgs
	    | _ -> assert false		 
	in 
	(env, max_size)
      )
      formulas (env, max_size)
  with EnoughLemmasAlready(env, max_size) -> env, max_size

let add_instance_info env orig = 
  match F.view orig with
    | F.Lemma _ -> env.add_inst orig
    | _ -> ()

let new_facts goal_directed env = 
  List.fold_left
    (fun acc ({Matching.trigger_formula=f; trigger_query = guard; 
	       trigger_age=age; trigger_dep=dep; trigger_orig=orig }, 
	      subst_list) ->
      List.fold_left
	(fun acc 
	  {Matching.sbs = sbs; 
	   sty = sty; 
	   gen = g; 
	   goal = b; 
	   s_term_orig = torig;
	   s_lem_orig = lorig; } ->
            let sbs = 
              Symbols.Map.fold
                (fun k t mp ->
                  let t = 
                    if List.length (CcX.class_of env.tbox t) <= 4 then t
                    else CcX.term_repr env.tbox t
                  in
                  Symbols.Map.add k t mp
                )sbs Symbols.Map.empty
            in
            let s = sbs, sty in
	    match guard with
	      | Some a when 
		  CcX.query (Literal.LT.apply_subst s a) env.tbox = No -> acc
	      | _ ->
		if goal_directed && not b then acc
		else
		  try
		    let nf = F.apply_subst s f in
		    if MF.mem nf env.gamma then acc else
		      let p = 
			{ f = nf;
			  age = 1+(max g age);
			  mf = true;
			  gf = b;
			  name = Some lorig;
			  from_terms = torig;
			  inv = false
			} in
		      add_instance_info env orig;
		      (p,dep)::acc
		  with Exit -> acc
	) 
	acc subst_list
    )
    [] (MM.query env.matching env.tbox)


let sort_facts =
  let rec size f = match F.view f with
    | F.Unit(f1,f2) -> max (size f1) (size f2)
    | _             -> F.size f
  in
  fun lf -> List.fast_sort (fun (p1,_) (p2,_) -> size p1.f - size p2.f) lf

type select = Select_predicates | Select_lemmas | Select_inversions 
    
(* predicate = true : consider only predicates for matching
   goal_directed = true : match only with terms from goal *)

let mround select ~goal_directed env max_size =
  Options.tool_req 2 "TR-Sat-Mround";
  Options.exec_timer_start Timers.TMatch;
  Debug.mround max_size;
  let axs = match select with
    | Select_predicates -> env.definitions
    | Select_lemmas -> env.lemmas
    | Select_inversions -> env.inversions
  in
  let env, max_size = mtriggers env axs max_size in
  let lf = new_facts goal_directed env in
  Options.exec_timer_pause Timers.TMatch;
  max_size, sort_facts lf

let is_literal f = match F.view f with F.Literal _ -> true | _ -> false

let extract_prop_model t = 
  let s = ref SF.empty in
  MF.iter 
    (fun f _ -> 
      if (complete_model () && is_literal f) || F.is_in_model f then
	s := SF.add f !s
    ) 
    t.gamma;
  !s

let print_prop_model fmt s =
  SF.iter (fprintf fmt "\n %a" F.print) s

let print_model ~header fmt t =
  Format.print_flush ();
  if header then fprintf fmt "\nModel\n@.";
  let pm = extract_prop_model t in
  if not (SF.is_empty pm) then begin
    fprintf fmt "Propositional:";
    print_prop_model fmt pm;
    fprintf fmt "\n@.";
  end;
  CcX.print_model fmt t.tbox


let _ =
  if not (model ()) then
    try
      Sys.set_signal Sys.sigalrm
	(Sys.Signal_handle (fun _ -> Options.exec_timeout ()))
    with Invalid_argument _ -> ()

let refresh_model_handler =
  if model () then
    fun t ->
      try
	Sys.set_signal Sys.sigalrm
	  (Sys.Signal_handle (fun _ ->
	    printf "%a@." (print_model ~header:true) t;
	    Options.exec_timeout ()))
      with Invalid_argument _ -> ()
  else fun _ -> ()

(* sat-solver *)

let elim {f=f} env = 
  if MF.mem f env.gamma then
    begin
      Options.tool_req 2 "TR-Sat-Bcp-Elim-1";
      true
    end
  else
    let el = match F.view f with 
      | F.Literal a -> CcX.query a env.tbox <> No
      | _ -> false
    in
    if el then 
      Options.tool_req 2 "TR-Sat-Assume-Let";
    el

let size_formula = 1_000_000

let red {f=f} env = 
  let nf = F.mk_not f in
  try 
    let r = Yes (MF.find nf env.gamma, CcX.cl_extract env.tbox) in
    Options.tool_req 2 "TR-Sat-Bcp-Red-1";
    r
  with Not_found -> 
    let r = match F.view nf with
      |	F.Literal a -> CcX.query a env.tbox
      | _ -> No
    in
    begin match r with 
      |	Yes _ -> Options.tool_req 2 "TR-Sat-Bcp-Red-2"
      | No -> ()
    end;
    r
      

let pred_def env f = 
  let ff = {f=f;age=0;name=None;mf=false;gf=false; from_terms=[]; inv=false} in
  Debug.assume ff Explanation.empty;
  { env with definitions = MF.add f (0,Ex.empty) env.definitions }



let add_dep f dep =
  match F.view f with 
    | F.Literal _ when proof () -> 
      if not (Ex.mem_as_bj f dep) then
	Ex.union (Ex.singleton ~bj:false f) dep
      else dep
    | F.Clause _ when proof () -> 
      Ex.union (Ex.singleton ~bj:false f) dep
    | _ -> dep
      

let rec add_dep_of_formula f dep =
  let dep = add_dep f dep in
  match F.view f with 
    | F.Unit (f1, f2) when proof () ->
      add_dep_of_formula f2 (add_dep_of_formula f1 dep)
    | _ -> dep


let rec assume env ({f=f;age=age;name=lem;mf=mf;gf=gf;inv=inv} as ff ,dep) =
  refresh_model_handler env;
  Options.exec_thread_yield ();
  try
    let dep = add_dep f dep in
    let dep_gamma = add_dep_of_formula f dep in
    (try 
       Debug.gamma env.gamma;
       let ex_nf = MF.find (F.mk_not f) env.gamma in
       Options.tool_req 2 "TR-Sat-Conflict-1";
       raise (IUnsat (Ex.union dep_gamma ex_nf, CcX.cl_extract env.tbox))
     with Not_found -> ());
    if MF.mem f env.gamma then
      begin
	Options.tool_req 2 "TR-Sat-Remove";
	env
      end
    else 
      begin
	let size = F.size f in
	if size > size_formula then env
	else
	  let env =
	    if mf && glouton () && size < size_formula then 
	      add_terms env (F.terms f) gf age lem ff.from_terms else env in
	  let env = { env with gamma = MF.add f dep_gamma env.gamma } in
	  Debug.assume ff dep;
	  match F.view f with
	    | F.Unit (f1, f2) ->
	      Options.tool_req 2 "TR-Sat-Assume-U";
	      let env = assume env ({ ff with f = f1}, dep) in
	      assume env ({ ff with f = f2}, dep) 
	    | F.Clause(f1,f2) -> 
	      Options.tool_req 2 "TR-Sat-Assume-C";
	      let p1 = { ff with f=f1 } in
	      let p2 = { ff with f=f2 } in
	      bcp { env with delta = (p1,p2,dep)::env.delta }

	    | F.Lemma l ->
	      Options.tool_req 2 "TR-Sat-Assume-Ax";
	      let age , dep = 
		try 
		  let age' , dep' = MF.find f env.lemmas in
		  min age age' , Ex.union dep dep' 
		with Not_found -> age , dep 
	      in
	      let env = 
		if inv then 
		  { env with inversions=MF.add f (age,dep) env.inversions }
		else
		  { env with lemmas=MF.add f (age,dep) env.lemmas }
	      in
	      bcp env

	    | F.Literal a ->
	      Options.tool_req 2 "TR-Sat-Assume-Lit";
	      let env = 
		if mf && size < size_formula then 
		  add_terms env (A.LT.terms_of a) gf age lem ff.from_terms
		else env 
	      in
	      let tbox, new_terms, cpt = CcX.assume a dep env.tbox in
	      let env = add_terms env new_terms gf age lem ff.from_terms in
	      steps := Int64.add (Int64.of_int cpt) !steps;
	      if steps_bound () <> -1 
		&& Int64.compare !steps (Int64.of_int (steps_bound ())) > 0 then 
		begin 
		  printf "Steps limit reached: %Ld@." !steps;
		  exit 1
		end;
	      let env = { env with tbox = tbox } in
	      bcp env

	    | F.Skolem{F.sko_subst=sigma; sko_f=f} -> 
	      Options.tool_req 2 "TR-Sat-Assume-Sko";
	      let f' = F.apply_subst sigma f in
	      assume env ({ ff with f=f'},dep)

            | F.Let {F.let_var=lvar; let_term=lterm; let_subst=s; let_f=lf} ->
	      Options.tool_req 2 "TR-Sat-Assume-Let";
              let f' = F.apply_subst s lf in
	      let id = F.id f' in
              let v = Symbols.Map.find lvar (fst s) in
              let env = assume env 
		({ ff with f=F.mk_lit (A.LT.make (A.Eq(v,lterm))) id},dep) 
	      in
              assume env ({ ff with f=f' },dep)
      end
  with Exception.Inconsistent (expl, classes) -> 
    Debug.inconsistent expl;
    Options.tool_req 2 "TR-Sat-Conflict-2";
    raise (IUnsat (expl, classes))
      
and bcp env =
  let cl , u = 
    List.fold_left 
      (fun (cl,u) ((f1,f2,d) as fd) -> 
        Debug.elim f1 f2;
	if elim f1 env || elim f2 env  then (cl,u)
	else 
          (Debug.red f1 f2;
	   match red f1 env with
	     | Yes (d1, c1) -> begin
	       match red f2 env with
		 | Yes (d2, c2) -> 
		   let expl = Ex.union (Ex.union d d1) d2 in
		   raise (Exception.Inconsistent (expl, c1@c2))
		 | No -> (cl,(f2,Ex.union d d1)::u)
	     end
	     | No -> 
	       match red f2 env with
		   Yes (d2, _) -> (cl,(f1,Ex.union d d2)::u)
		 | No -> fd::cl , u)
      ) ([],[]) env.delta
  in
  List.fold_left assume {env with delta=cl} u
    
let rec unsat_rec env fg stop max_size = 
  try
    if stop < 0 then raise (I_dont_know env);
    back_tracking (assume env fg) stop max_size
  with IUnsat (d, classes) ->
    Debug.bottom classes;
    Debug.unsat (); 
    d

and back_tracking env stop max_size = match env.delta with
  | []  when stop >= 0  -> 
    let _ , l2 = 
      mround Select_predicates ~goal_directed:false env max_max_size in 
    let env = List.fold_left assume env l2 in

    let max_size , l1 = 
      mround Select_lemmas ~goal_directed:false env max_size in 

    let env = List.fold_left assume env l1 in

    let env = 
      List.fold_left 
	(fun env ({f=f; age=g; name=lem; gf=gf} as ff,_) -> 
	  add_terms env (F.terms f) gf g lem ff.from_terms) env l1 
    in
    (match l1, l2 with
      | [], [] ->
	if all_models () then 
	  begin
	    let m = extract_prop_model env in
	    Format.printf "--- SAT ---\n";
	    Format.printf "%a@." print_prop_model m;
	    raise (IUnsat (Ex.make_deps m, []))
	  end;
	raise (Sat env)
      | l1, l2 -> 
	back_tracking 
	  (List.fold_left assume  (List.fold_left assume env l2) l1) 
	  (stop-1) (max_size + b_max_size))
  | [] ->
    raise (I_dont_know env)
  | (a,b,d)::l ->
    let {f=f;age=g;name=lem;mf=mf} = a in
    Debug.decide f;
    let dep = unsat_rec {env with delta=l} (a,Ex.singleton f) stop max_size in
    Debug.unsat_rec dep;
    try
      let dep' = Ex.remove f dep in
      Debug.backtracking (F.mk_not f);
      Options.tool_req 2 "TR-Sat-Decide";
      unsat_rec
	(assume {env with delta=l} (b, Ex.union d dep'))
	({a with f=F.mk_not f},dep') stop max_size
    with Not_found -> 
      Debug.backjumping (F.mk_not f);
      Options.tool_req 2 "TR-Sat-Backjumping";
      dep
	
let unsat env fg =
  try
    let env = assume env (fg, Ex.empty) in
    let env = 
      if not fg.mf then env
      else add_terms env (F.terms fg.f) fg.gf fg.age fg.name fg.from_terms
    in

    let _ , l = 
      mround Select_inversions ~goal_directed:false env max_max_size in
    let env = List.fold_left assume env l in

    let _ , l = 
      mround Select_predicates ~goal_directed:false env max_max_size in
    let env = List.fold_left assume env l in

    let _ , l = 
      mround Select_lemmas ~goal_directed:true env max_max_size in
    let env = List.fold_left assume env l in

    back_tracking env (stop_bound ()) 100
  with IUnsat (dep, classes) ->
    Debug.bottom classes;
    Debug.unsat ();
    dep

let assume env fg = 
  try assume env (fg,Ex.empty) 
  with IUnsat (d, classes) ->
    Debug.bottom classes;
    raise (Unsat d)

let unsat env fg = 
  if profiling() then
    try 
      Options.exec_timer_start Timers.TSat;
      let env = unsat env fg in
      Options.exec_timer_pause Timers.TSat;
      env
    with e -> 
      Options.exec_timer_pause Timers.TSat;
      raise e
  else unsat env fg

let assume env fg = 
  if profiling() then
    try 
      Options.exec_timer_start Timers.TSat;
      let env = assume env fg in
      Options.exec_timer_pause Timers.TSat;
      env
    with e -> 
      Options.exec_timer_pause Timers.TSat;
      raise e
  else assume env fg


let empty () = { 
  gamma = MF.empty;
  delta = [] ;
  tbox = CcX.empty (); 
  lemmas = MF.empty ; 
  inversions = MF.empty ; 
  matching = MM.empty;
  definitions = MF.empty;
  add_inst = fun _ -> ();
} 

let empty_with_inst add_inst =
  { (empty ()) with add_inst = add_inst }

let start () = steps := 0L
let stop () = !steps