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

open Format
open Options
open Exception
open Sig

module type S = sig
  type t

  module R : Sig.X

  val empty :  t
  val add : t -> Term.t -> t * Literal.LT.t list

  val mem : t -> Term.t -> bool

  val find : t -> Term.t -> R.r * Explanation.t
  val find_r : t -> R.r -> R.r * Explanation.t
    
  val union : 
    t -> R.r -> R.r -> Explanation.t -> 
    t * (R.r * (R.r * R.r * Explanation.t) list * R.r) list

  val distinct : t -> R.r list -> Explanation.t -> t

  val are_equal : t -> Term.t -> Term.t -> Sig.answer
  val are_distinct : t -> Term.t -> Term.t -> Sig.answer
  val already_distinct : t -> R.r list -> bool
    
  val class_of : t -> Term.t -> Term.t list
  val cl_extract : t -> Term.Set.t list
  val model : t -> 
    (R.r * Term.t list * (Term.t * R.r) list) list * (Term.t list) list
  val print : Format.formatter -> t -> unit
  val term_repr : t -> Term.t -> Term.t
    
end
  
module Make ( R : Sig.X ) = struct

  module Ac = Ac.Make(R)
  module L  = List
  module HS = Hstring
  module Ex = Explanation
  module R = R
  module Sy= Symbols
  module T = Term
  module F = Formula
  module MapT = Term.Map
  module SetT = Term.Set
  module SetF = Formula.Set
    
  module Lit = Literal.Make(struct type t = R.r include R end)
  module MapL = Lit.Map

  module MapR = Map.Make(struct type t = R.r let compare = R.compare end)
    
  module SetR = Set.Make(struct type t = R.r let compare = R.compare end)

  module SetRR = Set.Make(struct 
    type t = R.r * R.r 
    let compare (r1, r1') (r2, r2') = 
      let c = R.compare r1 r2 in
      if c <> 0 then c 
      else  R.compare r1' r2'
  end)

  module SetAc = Set.Make(struct type t = Ac.t let compare = Ac.compare end)

  module SetRL = Set.Make
    (struct 
      type t = Ac.t * R.r * Ex.t
      let compare (ac1,_,_) (ac2,_,_)= Ac.compare ac1 ac2
     end)

  module RS = struct
    include Map.Make(struct type t = Sy.t let compare = Sy.compare end)
      
    let find k m = 
      try find k m with Not_found -> SetRL.empty

    let add_rule (({h=h},_,_) as rul) mp =
      add h (SetRL.add rul (find h mp)) mp

    let remove_rule (({h=h},_,_) as rul) mp =
      add h (SetRL.remove rul (find h mp)) mp

  end


  type t = { 

    (* term -> [t] *)
    make : R.r MapT.t; 
    
    (* representative table *)
    repr : (R.r * Ex.t) MapR.t; 
    
    (* r -> class (of terms) *)
    classes : SetT.t MapR.t;
    
    (*associates each value r with the set of semantical values whose
      representatives contains r *)
    gamma : SetR.t MapR.t; 
    
    (* the disequations map *)
    neqs: Ex.t MapL.t MapR.t; 
    
    (*AC rewrite system *)
    ac_rs : SetRL.t RS.t;
  }
      
  let empty = { 
    make  = MapT.empty; 
    repr = MapR.empty;
    classes = MapR.empty; 
    gamma = MapR.empty;
    neqs = MapR.empty;
    ac_rs = RS.empty
  }

  exception Found_term of T.t

  (* hack: would need an inverse map from semantic values to terms *)
  let terms_of_distinct env l = match Lit.view l with
    | Literal.Distinct (false, rl) ->
      let lt =
        List.fold_left (fun acc r -> 
	  try 
	    let cl = MapR.find r env.classes in
	    SetT.iter (fun t -> 
	      if R.equal (MapT.find t env.make) r then
		raise (Found_term t)) cl;
	    acc
	  with 
	    | Found_term t -> t :: acc
	    | Not_found -> acc) [] rl
      in
      let rec distrib = function
	| x :: r -> (distrib r) @ 
	  (List.map (fun y -> SetT.add x (SetT.singleton y)) r)  
	| [] -> []
      in
      distrib lt

    | _ -> assert false


  let cl_extract env =
    if bottom_classes () then
      let classes = MapR.fold (fun _ cl acc -> cl :: acc) env.classes [] in
      MapR.fold (fun _ ml acc -> 
	MapL.fold (fun l _ acc -> (terms_of_distinct env l) @ acc) ml acc
      ) env.neqs classes
    else []
      

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct

    let rs_print fmt = SetR.iter (fprintf fmt "\t%a@." R.print)
    let lm_print fmt = 
      MapL.iter (fun k dep -> fprintf fmt "%a %a" Lit.print k Ex.print dep)

    let t_print fmt = SetT.iter (fprintf fmt "%a " T.print)
      
    let pmake fmt m = 
      fprintf fmt "[.] map:\n";
      MapT.iter (fun t r -> fprintf fmt "%a -> %a\n" T.print t R.print r) m
	
    let prepr fmt m = 
      fprintf fmt "------------- UF: Representatives map ----------------@.";
      MapR.iter 
	(fun r (rr,dep) -> 
	  fprintf fmt "%a --> %a %a\n" R.print r R.print rr Ex.print dep) m

    let prules fmt s = 
      fprintf fmt "------------- UF: AC rewrite rules ----------------------@.";
      RS.iter
	(fun k srl -> 
	  SetRL.iter
	    (fun (ac,d,dep)-> fprintf fmt "%a ~~> %a %a\n" 
              R.print (R.ac_embed ac) R.print d Ex.print dep
            )srl 
        )s
	
    let pclasses fmt m = 
      fprintf fmt "------------- UF: Class map --------------------------@.";
      MapR.iter 
	(fun k s -> fprintf fmt "%a -> %a\n" R.print k Term.print_list 
	  (SetT.elements s)) m

    let pgamma fmt m = 
      fprintf fmt "------------- UF: Gamma map --------------------------@.";
      MapR.iter (fun k s -> fprintf fmt "%a -> \n%a" R.print k rs_print s) m 
	
    let pneqs fmt m = 
      fprintf fmt "------------- UF: Disequations map--------------------@.";
      MapR.iter (fun k s -> fprintf fmt "%a -> %a\n" R.print k lm_print s) m

    let all fmt env = 
      if debug_uf () then
	begin
	  fprintf fmt "-------------------------------------------------@.";
	  fprintf fmt "%a %a %a %a %a" 
            pmake env.make 
            prepr env.repr 
            prules env.ac_rs 
            pclasses env.classes
            pneqs env.neqs;
	  fprintf fmt "-------------------------------------------------@."
	end

    let lookup_not_found t =
      fprintf fmt "Uf: %a Not_found in env@." T.print t
        
        
    let canon_of r rr =
      if rewriting () && verbose () then 
        fprintf fmt "canon %a = %a@." R.print r R.print rr
     
    let init_leaf p = 
      if debug_uf () then fprintf fmt "init_leaf: %a@." R.print p
     
    let critical_pair rx ry =
      if debug_uf () then
        fprintf fmt "[uf] critical pair: %a = %a@." R.print rx R.print ry

    let collapse_mult g2 d2 = 
      if debug_ac () then
        fprintf fmt "[uf] collapse *: %a = %a@."
	  R.print g2 R.print d2
          

    let collapse g2 d2 = 
      if debug_ac () then
        fprintf fmt "[uf] collapse: %a = %a@."
	  R.print g2 R.print d2
          
    let compose p v g d = 
      if debug_ac () then
	Format.eprintf "Compose : %a -> %a on %a and %a@." 
	  R.print p R.print v
	  Ac.print g R.print d
          
    let x_solve rr1 rr2 dep =
      if debug_uf () then 
        printf "[uf] x-solve: %a = %a %a@."
	  R.print rr1 R.print rr2 Ex.print dep
          
    let ac_solve p v dep =
      if debug_uf () then 
        printf "[uf] ac-solve: %a |-> %a %a@." R.print p R.print v Ex.print dep
      
    let ac_x r1 r2 =
      if debug_uf () then 
	printf "[uf] ac(x): delta (%a) = delta (%a)@." 
	  R.print r1 R.print r2
          
    let distinct d =
      if debug_uf () then fprintf fmt "[uf] distinct %a@." Lit.print d
      
    let are_distinct t1 t2 =
      if debug_uf () then
        printf " [uf] are_distinct %a %a @." T.print t1 T.print t2

  end
  (*BISECT-IGNORE-END*)
    
  module Env = struct

    let mem env t = MapT.mem t env.make
      
    let lookup_by_t t env =
      Options.exec_thread_yield ();
      try MapR.find (MapT.find t env.make) env.repr
      with Not_found -> 
	Debug.lookup_not_found t; 
	assert false (*R.make t, Ex.empty*) (* XXXX *)
	  
    let lookup_by_r r env = 
      Options.exec_thread_yield ();
      try MapR.find r env.repr with Not_found -> r, Ex.empty
	
    let lookup_for_neqs env r =
      Options.exec_thread_yield ();
      try MapR.find r env.neqs with Not_found -> MapL.empty
	
    let add_to_classes t r classes =  
      MapR.add r 
	(SetT.add t (try MapR.find r classes with Not_found -> SetT.empty))
	classes
	
    let update_classes c nc classes = 
      let s1 = try MapR.find c classes with Not_found -> SetT.empty in
      let s2 = try MapR.find nc classes with Not_found -> SetT.empty in
      MapR.remove c (MapR.add nc (SetT.union s1 s2) classes)
	
    let add_to_gamma r c gamma =
      Options.exec_thread_yield ();
      L.fold_left
	(fun gamma x -> 
	  let s = try MapR.find x gamma with Not_found -> SetR.empty in
	  MapR.add x (SetR.add r s) gamma) gamma (R.leaves c)
	
    (* r1 = r2 => neqs(r1) \uplus neqs(r2) *)
    let update_neqs r1 r2 dep env = 
      let nq_r1 = lookup_for_neqs env r1 in
      let nq_r2 = lookup_for_neqs env r2 in
      let mapl = 
	MapL.fold
	  (fun l1 ex1 mapl ->  
	    try 
	      let ex2 = MapL.find l1 mapl in
	      let ex = Ex.union (Ex.union ex1 ex2) dep in (* VERIF *)
	      Options.tool_req 3 "TR-CCX-Congruence-Conflict";
	      raise (Inconsistent (ex, cl_extract env))
	    with Not_found -> 
	      MapL.add l1 (Ex.union ex1 dep) mapl) 
	  nq_r1 nq_r2
      in
      MapR.add r2 mapl (MapR.add r1 mapl env.neqs)

    let disjoint_union l_1 l_2 = 
      let rec di_un (l1,c,l2) (l_1,l_2)= 
	Options.exec_thread_yield ();
        match l_1,l_2 with
	  | [],[] -> l1, c, l2
	  | l, [] -> di_un (l @ l1,c,l2) ([],[])
	  | [], l -> di_un (l1,c,l @ l2) ([],[])
	  | (a,m)::r, (b,n)::s ->
	    let cmp = R.compare a b in
	    if cmp = 0 then
	      if m = n then di_un (l1,(a,m)::c,l2) (r,s)
	      else if m > n then di_un ((a,m-n)::l1,(a,n)::c,l2) (r,s)
	      else di_un (l1,(b,n)::c,(b,n-m)::l2) (r,s)
	      else if cmp > 0 then di_un ((a,m)::l1,c,l2) (r,(b,n)::s)
	      else di_un (l1,c,(b,n)::l2) ((a,m)::r,s)
      in di_un ([],[],[]) (l_1,l_2)


    (* Debut : Code pour la mise en forme normale modulo env *)
    exception List_minus_exn
    let list_minus l_1 l_2 = 
      let rec di_un l1 l_1 l_2 = 
        match l_1, l_2 with
	    [],[] -> l1
	  | l, [] -> l @ l1
	  | [], l -> raise List_minus_exn
	  | (a,m)::r, (b,n)::s ->
	    let cmp = R.compare a b in
	    if cmp = 0 then
	      if m = n then di_un l1 r s
	      else if m > n then di_un ((a,m-n)::l1) r s
	      else raise List_minus_exn
	      else if cmp > 0 then di_un ((a,m)::l1) r ((b,n)::s)
	      else raise List_minus_exn
      in di_un [] l_1 l_2
      
    let apply_rs r rls = 
      let fp = ref true in
      let r = ref r in
      let ex = ref Ex.empty in
      let rec apply_rule ((p, v, dep) as rul) =
	let c = Ac.compare !r p in
	if c = 0 then begin
          r := {!r with l=[v, 1]};
          ex := Ex.union !ex dep
        end
	else if c < 0 then raise Exit
	else 
          try 
            r := {!r with l = Ac.add !r.h (v, 1) (list_minus !r.l p.l)};
            ex := Ex.union !ex dep;
	    fp := false;
            apply_rule rul
          with List_minus_exn -> ()
      in
      let rec fixpoint () = 
	Options.exec_thread_yield ();
        (try SetRL.iter apply_rule rls with Exit -> ());
	if !fp then !r, !ex else (fp := true; fixpoint ())
      in fixpoint()

    let filter_leaves r = 
      L.fold_left 
	(fun (p,q) r -> match R.ac_extract r with 
	  | None    -> SetR.add r p, q
	  | Some ac -> p, SetAc.add ac q
	)(SetR.empty,SetAc.empty) (R.leaves r)
	
    let canon_empty st env = 	
      SetR.fold
	(fun p ((z, ex) as acc) -> 
          let q, ex_q = lookup_by_r p env in 
	  if R.equal p q then acc else (p,q)::z, Ex.union ex_q ex)
	st ([], Ex.empty)

    let canon_ac st env = 
      SetAc.fold
	(fun ac (z,ex) ->
	  let rac, ex_ac = apply_rs ac (RS.find ac.h env.ac_rs) in
	  if Ac.compare ac rac = 0 then z, ex
	  else (R.color ac, R.color rac) :: z, Ex.union ex ex_ac)
        st ([], Ex.empty)
	
    let canon_aux rx = List.fold_left (fun r (p,v) -> R.subst p v r) rx
      
    let rec canon env r ex_r = 
      let se, sac = filter_leaves r in
      let subst, ex_subst = canon_empty se env in
      let subst_ac, ex_ac = canon_ac sac env in (* explications? *)
      let r2 = canon_aux (canon_aux r subst_ac) subst in
      let ex_r2 = Ex.union (Ex.union ex_r ex_subst) ex_ac in
      if R.equal r r2 then r2, ex_r2 else canon env r2 ex_r2

    let normal_form env r =
      let rr, ex = canon env r Ex.empty in
      Debug.canon_of r rr;
      rr,ex

    (* Fin : Code pour la mise en forme normale modulo env *)


    let find_or_normal_form env r =
      Options.exec_thread_yield ();
      try MapR.find r env.repr with Not_found -> normal_form env r

    let init_leaf env p =
      Debug.init_leaf p;
      let in_repr = MapR.mem p env.repr in
      { env with
	repr    = 
	  if in_repr then env.repr 
	  else MapR.add p (p, Ex.empty) env.repr;
	classes = 
	  if MapR.mem p env.classes then env.classes
	  else update_classes p p env.classes;
	gamma   = 
	  if in_repr then env.gamma
	  else add_to_gamma p p env.gamma ;
	neqs    =  
	  if MapR.mem p env.neqs then env.neqs 
	  else update_neqs p p Ex.empty env } 
        
    let init_term env t = 
      let mkr, ctx = R.make t in
      let rp, ex = normal_form env mkr in
      {env with
	make    = MapT.add t mkr env.make; 
	repr    = MapR.add mkr (rp,ex) env.repr;
	classes = add_to_classes t rp env.classes;
	gamma   = add_to_gamma mkr rp env.gamma;
	neqs    = 
	  if MapR.mem rp env.neqs then env.neqs (* pourquoi ce test *)
	  else MapR.add rp MapL.empty env.neqs}, ctx


    let head_cp eqs env (({h=h} as ac), v, dep) = 
      try (*if RS.mem h env.ac_rs then*)
        SetRL.iter
	  (fun (g, d, dep_rl) ->
	    match disjoint_union ac.l g.l with
	      | _  , [] , _  -> ()
	      | l1 , cm , l2 -> 
		let rx = R.color {ac with l = Ac.add h (d,1) l1} in
		let ry = R.color {g  with l = Ac.add h (v,1) l2} in
                Debug.critical_pair rx ry;
                if not (R.equal rx ry) then 
                  Queue.push (rx, ry, Ex.union dep dep_rl) eqs)
	  (RS.find h env.ac_rs)
      with Not_found -> assert false
	
    let comp_collapse eqs env (p, v, dep) = 
      RS.fold
	(fun h rls env ->
          SetRL.fold
	    (fun ((g, d, dep_rl) as rul) env ->
	      Options.exec_thread_yield ();
	      let env = {env with ac_rs = RS.remove_rule rul env.ac_rs} in
	      let gx = R.color g in
	      let g2, ex_g2 = normal_form env (Ac.subst p v g) in
	      let d2, ex_d2 = normal_form env (R.subst p v d) in
              if R.compare g2 d2 <= 0 then begin
                Debug.collapse_mult g2 d2;
                let ex = Ex.union 
		  (Ex.union ex_g2 ex_d2) (Ex.union dep_rl dep) in
                Queue.push (g2, d2, ex) eqs;
	        env
              end
              else
	        if R.equal g2 gx then (* compose *)
		  begin
                    Debug.compose p v g d;
                    let ex = Ex.union ex_d2 (Ex.union dep_rl dep) in
	            {env with ac_rs = RS.add_rule (g,d2, ex) env.ac_rs}
		  end
	        else (* collapse *)
                  begin
                    Debug.collapse g2 d2;
                    let ex = 
                      Ex.union 
		        (Ex.union ex_g2 ex_d2) (Ex.union dep_rl dep) in
                    Queue.push (g2, d2, ex) eqs;
	            env
                  end
	    ) rls env
	) env.ac_rs env
	
    (* TODO explications: ajout de dep dans ac_rs *)
    let apply_sigma_ac eqs env ((p, v, dep) as sigma) = 
      match R.ac_extract p with
	| None -> 
	  comp_collapse eqs env sigma
	| Some r -> 
	  let env = {env with ac_rs = RS.add_rule (r, v, dep) env.ac_rs} in
	  let env = comp_collapse eqs env sigma in
	  head_cp eqs env (r, v, dep);
          env
	    
    let update_aux dep set env= 
      SetRR.fold 
	(fun (rr, nrr) env -> 
	  { env with
	    neqs = update_neqs rr nrr dep env ;
	    classes = update_classes rr nrr env.classes})
	set env

    let apply_sigma_uf env (p, v, dep) =
      assert (MapR.mem p env.gamma);
      let use_p = MapR.find p env.gamma in
      try 
	let env, tch, neqs_to_up = SetR.fold 
	  (fun r (env, touched, neqs_to_up) ->
	    Options.exec_thread_yield (); 
	    let rr, ex = MapR.find r env.repr in
	    let nrr = R.subst p v rr in
	    if R.equal rr nrr then env, touched, neqs_to_up
	    else 
	      let ex  = Ex.union ex dep in
              let env = 
		{env with
		  repr = MapR.add r (nrr, ex) env .repr;
		  gamma = add_to_gamma r nrr env.gamma } 
	      in
	      env, (r, nrr, ex)::touched, SetRR.add (rr, nrr) neqs_to_up
	  ) use_p (env, [], SetRR.empty) in
	(* Correction : Do not update neqs twice for the same r *)
	update_aux dep neqs_to_up env, tch 
	  
      with Not_found -> assert false

    let up_uf_rs dep env tch =
      if RS.is_empty env.ac_rs then env, tch
      else
	let env, tch, neqs_to_up = MapR.fold
	  (fun r (rr,ex) (env,tch,neqs_to_up) ->
	    Options.exec_thread_yield ();
	    let nrr, ex_nrr = normal_form env rr in
	    if R.equal nrr rr then env, tch, neqs_to_up
	    else 
	      let ex = Ex.union ex ex_nrr in
              let env = 
		{env with
	          repr = MapR.add r (nrr, ex) env.repr;
	          gamma = add_to_gamma r nrr env.gamma }
              in
              env, (r,[r, nrr, ex],nrr)::tch, SetRR.add (rr, nrr) neqs_to_up
	  ) env.repr (env, tch, SetRR.empty)
        in 
        (* Correction : Do not update neqs twice for the same r *)
	update_aux dep neqs_to_up env, tch 
	  
    let apply_sigma eqs env tch ((p, v, dep) as sigma) = 
      let env = init_leaf env p in
      let env = init_leaf env v in
      let env = apply_sigma_ac eqs env sigma in
      let env, touched = apply_sigma_uf env sigma in 
      up_uf_rs dep env ((p, touched, v) :: tch)
	
  end
    
  let add env t = 
    Options.tool_req 3 "TR-UFX-Add";
    if MapT.mem t env.make then env, [] else Env.init_term env t

  let ac_solve eqs dep (env, tch) (p, v) = 
    (* pourquoi recuperer le representant de rv? r = rv d'apres testopt *)
    Debug.ac_solve p v dep;
    assert (not (Options.enable_assertions()) ||
              let rp, _ = Env.find_or_normal_form env p in R.equal p rp);
    let rv, ex_rv = Env.find_or_normal_form env v in
    assert (not (Options.enable_assertions()) ||
              let rv, _ = Env.find_or_normal_form env v in R.equal v rv);
    let dep = Ex.union ex_rv dep in
    Env.apply_sigma eqs env tch (p, rv, dep)

  let x_solve env r1 r2 dep = 
    let rr1, ex_r1 = Env.find_or_normal_form env r1 in
    let rr2, ex_r2 = Env.find_or_normal_form env r2 in
    let dep = Ex.union dep (Ex.union ex_r1 ex_r2) in
    Debug.x_solve rr1 rr2 dep;
    if R.equal rr1 rr2 then begin
      Options.tool_req 3 "TR-CCX-Remove";
      [] (* Remove rule *)
    end
    else 
      begin
	ignore (Env.update_neqs rr1 rr2 dep env);
        try R.solve rr1 rr2 
	with Unsolvable ->
	  Options.tool_req 3 "TR-CCX-Congruence-Conflict";
	  raise (Inconsistent (dep, cl_extract env))
      end
        
  let rec ac_x eqs env tch = 
    if Queue.is_empty eqs then env, tch
    else 
      let r1, r2, dep = Queue.pop eqs in
      Debug.ac_x r1 r2;
      let sbs = x_solve env r1 r2 dep in
      let env, tch = List.fold_left (ac_solve eqs dep) (env, tch) sbs in
      if debug_uf () then Debug.all fmt env;
      ac_x eqs env tch
        
  let union env r1 r2 dep =
    Options.tool_req 3 "TR-UFX-Union";
    let equations = Queue.create () in 
    Queue.push (r1,r2, dep) equations;
    ac_x equations env []

  let rec distinct env rl dep =
    Debug.all fmt env;
    let d = Lit.make (Literal.Distinct (false,rl)) in
    Debug.distinct d;
    let env, _, newds = 
      List.fold_left
	(fun (env, mapr, newds) r -> 
	  Options.exec_thread_yield ();
	  let rr, ex = Env.find_or_normal_form env r in 
	  try
	    let exr = MapR.find rr mapr in
	    Options.tool_req 3 "TR-CCX-Distinct-Conflict";
	    raise (Inconsistent ((Ex.union ex exr), cl_extract env))
	  with Not_found ->
	    let uex = Ex.union ex dep in
	    let mdis = 
	      try MapR.find rr env.neqs with Not_found -> MapL.empty in
	    let mdis = 
	      try 
		MapL.add d (Ex.merge uex (MapL.find d mdis)) mdis
	      with Not_found -> 
		MapL.add d uex mdis
	    in
	    let env = Env.init_leaf env rr in
            let env = {env with neqs = MapR.add rr mdis env.neqs} in
            env, MapR.add rr uex mapr, (rr, ex, mapr)::newds
	)
	(env, MapR.empty, [])
	rl
    in
    List.fold_left 
      (fun env (r1, ex1, mapr) -> 
	MapR.fold (fun r2 ex2 env -> 
	  let ex = Ex.union ex1 (Ex.union ex2 dep) in
	  try match R.solve r1 r2 with
	    | [a, b] -> 
	      if (R.equal a r1 && R.equal b r2) ||
		(R.equal a r2 && R.equal b r1) then env
	      else
		distinct env [a; b] ex
	    | []  -> 
	      Options.tool_req 3 "TR-CCX-Distinct-Conflict";
	      raise (Inconsistent (ex, cl_extract env)) 
	    | _   -> env
	  with Unsolvable -> env) mapr env)
      env newds

      
  let are_equal env t1 t2 = 
    let r1, ex_r1 = Env.lookup_by_t t1 env in
    let r2, ex_r2 = Env.lookup_by_t t2 env in
    if R.equal r1 r2 then
      Yes (Ex.union ex_r1 ex_r2, cl_extract env)
    else No

  let are_distinct env t1 t2 = 
    Debug.are_distinct t1 t2;
    let r1, ex_r1 = Env.lookup_by_t t1 env in
    let r2, ex_r2 = Env.lookup_by_t t2 env in
    try
      ignore (union env r1 r2 (Ex.union ex_r1 ex_r2));
      No
    with Inconsistent (ex, classes) -> Yes (ex, classes)

  let already_distinct env lr =
    let d = Lit.make (Literal.Distinct (false,lr)) in
    try
      List.iter (fun r ->
  	let mdis = MapR.find r env.neqs in
  	ignore (MapL.find d mdis)
      ) lr;
      true
    with Not_found -> false

  let mapt_choose m =
    let r = ref None in
    (try 
       MapT.iter (fun x rx -> 
	 r := Some (x, rx); raise Exit
       ) m 
     with Exit -> ());
    match !r with Some b -> b | _ -> raise Not_found

  let model env =
    let eqs =
      MapR.fold (fun r cl acc ->
    	let l, to_rel =
    	  List.fold_left (fun (l, to_rel) t ->
    	    let rt = MapT.find t env.make in
    	    if complete_model () || T.is_in_model t then
    	      if rt <> r then t::l, (t,rt)::to_rel
    	      else l, (t,rt)::to_rel
    	    else l, to_rel
    	  ) ([], []) (SetT.elements cl) in
    	(r, l, to_rel)::acc
      ) env.classes []
    in
    let rec extract_neqs acc makes =
      try
	let x, rx = mapt_choose makes in
	let makes = MapT.remove x makes in
	let acc =
	  if complete_model () || T.is_in_model x then
	    MapT.fold (fun y ry acc ->
	      if (complete_model () || T.is_in_model y)
		&& (already_distinct env [rx; ry] 
		    || already_distinct env [ry; rx])
	      then [y; x]::acc
	      else acc
	    ) makes acc
	  else acc
	in extract_neqs acc makes
      with Not_found -> acc
    in
    let neqs = extract_neqs [] env.make in
    eqs, neqs


  let find env t = 
    Options.tool_req 3 "TR-UFX-Find";
    Env.lookup_by_t t env

  let find_r = 
    Options.tool_req 3 "TR-UFX-Find";
    Env.find_or_normal_form

  let print = Debug.all 

  let mem = Env.mem

  let class_of env t = 
    try 
      let rt, _ = MapR.find (MapT.find t env.make) env.repr in
      MapR.find rt env.classes
    with Not_found -> SetT.singleton t

  let term_repr uf t = 
    let st = class_of uf t in
    SetT.fold
      (fun s t ->
        let c = 
          let c = (T.view s).T.depth - (T.view t).T.depth in
          if c <> 0 then c
          else T.compare s t
        in
        if c < 0 then s else t
      ) st t

  let class_of env t = SetT.elements (class_of env t)

end