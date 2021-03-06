(******************************************************************************)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2015 --- OCamlPro                                   *)
(*     This file is distributed under the terms of the CeCILL-C licence       *)
(******************************************************************************)

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
open Typed
open Commands

module T = Term
module F = Formula
module A = Literal
module Sy = Symbols

let ale = Hstring.make "<="
let alt = Hstring.make "<"

let varset_of_list =
  List.fold_left
    (fun acc (s,ty) ->
      Term.Set.add (Term.make s [] (Ty.shorten ty)) acc) Term.Set.empty

let rec make_term {c = { tt_ty = ty; tt_desc = tt }} =
  let ty = Ty.shorten ty in
  match tt with
    | TTconst Ttrue ->
      T.vrai
    | TTconst Tfalse ->
      T.faux
    | TTconst Tvoid ->
      T.void
    | TTconst (Tint i) ->
      T.int i
    | TTconst (Treal n) ->
      T.real (Num.string_of_num n)
    | TTconst (Tbitv bt) ->
      T.bitv bt ty
    | TTvar s ->
      T.make s [] ty
    | TTapp (s, l) ->
      T.make s (List.map make_term l) ty
    | TTinfix (t1, s, t2) ->
      T.make s [make_term t1;make_term t2] ty
    | TTprefix ((Sy.Op Sy.Minus) as s, n) ->
      let t1 = if ty = Ty.Tint then T.int "0" else T.real "0"  in
      T.make s [t1; make_term n] ty
    | TTprefix _ ->
      assert false
    | TTget (t1, t2) ->
      T.make (Sy.Op Sy.Get) [make_term t1; make_term t2] ty
    | TTset (t1, t2, t3) ->
      let t1 = make_term t1 in
      let t2 = make_term t2 in
      let t3 = make_term t3 in
      T.make (Sy.Op Sy.Set) [t1; t2; t3] ty
    | TTextract (t1, t2, t3) ->
      let t1 = make_term t1 in
      let t2 = make_term t2 in
      let t3 = make_term t3 in
      T.make (Sy.Op Sy.Extract) [t1; t2; t3] ty
    | TTconcat (t1, t2) ->
      T.make (Sy.Op Sy.Concat) [make_term t1; make_term t2] ty
    | TTdot (t, s) ->
      T.make (Sy.Op (Sy.Access s)) [make_term t] ty
    | TTrecord lbs ->
      let lbs = List.map (fun (_, t) -> make_term t) lbs in
      T.make (Sy.Op Sy.Record) lbs ty
    | TTlet (s, t1, t2) ->
      let t1 = make_term t1 in
      let subst = Sy.Map.add s t1 Sy.Map.empty, Ty.esubst in
      let t2 = make_term t2 in
      T.apply_subst subst t2
    | TTnamed(lbl, t) ->
      let t = make_term t in
      T.add_label lbl t;
      t

let make_trigger = function
  | [{c={ tt_desc = TTapp(s, t1::t2::l)}}]
      when Sy.equal s Sy.fake_eq ->
    let trs = List.filter (fun t -> not (List.mem t l)) [t1; t2] in
    let trs = List.map make_term trs in
    let lit = A.LT.mk_eq (make_term t1) (make_term t2) in
    trs, Some lit

  | [{c={ tt_desc = TTapp(s, t1::t2::l) } }]
      when Sy.equal s Sy.fake_neq ->
    let trs = List.filter (fun t -> not (List.mem t l)) [t1; t2] in
    let trs = List.map make_term trs in
    let lit = A.LT.mk_distinct false [make_term t1; make_term t2] in
    trs, Some lit

  | [{c={ tt_desc = TTapp(s, t1::t2::l) } }]
      when Sy.equal s Sy.fake_le ->
    let trs = List.filter (fun t -> not (List.mem t l)) [t1; t2] in
    let trs = List.map make_term trs in
    let lit =
      A.LT.mk_builtin true ale [make_term t1; make_term t2]
    in
    trs, Some lit

  | [{c={ tt_desc = TTapp(s, t1::t2::l) } }]
      when Sy.equal s Sy.fake_lt ->
    let trs = List.filter (fun t -> not (List.mem t l)) [t1; t2] in
    let trs = List.map make_term trs in

    let lit =
      A.LT.mk_builtin true alt [make_term t1; make_term t2]
    in
    trs, Some lit

  | lt -> List.map make_term lt, None

let make_form name_base f loc =
  let name_tag = ref 0 in
  let rec make_form acc c id =
    match c with
    | TFatom a ->
      let a , lit = match a.c with
	| TAtrue ->
	  A.LT.vrai , A.LT.vrai::acc
	| TAfalse ->
	  A.LT.faux , A.LT.faux::acc
	| TAeq [t1;t2] ->
	  let lit = A.LT.mk_eq (make_term t1) (make_term t2) in
	  lit , lit::acc
	| TApred t ->
	  let lit = A.LT.mk_pred (make_term t) false in
	  lit , lit::acc
	| TAneq lt | TAdistinct lt ->
	  let lt = List.map make_term lt in
	  let lit = A.LT.mk_distinct false lt in
	  lit , lit::acc
	| TAle [t1;t2] ->
	  let lit =
	    A.LT.mk_builtin true ale [make_term t1;make_term t2]
	  in lit , lit::acc
 	| TAlt [t1;t2] ->
	  begin match t1.c.tt_ty with
	    | Ty.Tint ->
	      let one =
		{c = {tt_ty = Ty.Tint;
		      tt_desc = TTconst(Tint "1")}; annot = t1.annot} in
	      let tt2 =
		T.make (Sy.Op Sy.Minus)
		  [make_term t2; make_term one] Ty.Tint in
	      let lit =
		A.LT.mk_builtin true ale [make_term t1; tt2]
	      in lit , lit::acc
	    | _ ->
	      let lit =
		A.LT.mk_builtin true alt [make_term t1; make_term t2]
	      in lit, lit::acc
	  end
	| TAbuilt(n,lt) ->
	  let lit = A.LT.mk_builtin true n (List.map make_term lt) in
	  lit , lit::acc
	| _ -> assert false
      in F.mk_lit a id, lit

    | TFop(((OPand | OPor) as op),[f1;f2]) ->
      let ff1 , lit1 = make_form acc f1.c f1.annot in
      let ff2 , lit2 = make_form lit1 f2.c f2.annot in
      let mkop = match op with
	| OPand -> F.mk_and ff1 ff2 id
	| _ -> F.mk_or ff1 ff2 id in
      mkop , lit2
    | TFop(OPimp,[f1;f2]) ->
      let ff1 , _ = make_form acc f1.c f1.annot in
      let ff2 , lit = make_form acc f2.c f2.annot in
      F.mk_imp ff1 ff2 id, lit
    | TFop(OPnot,[f]) ->
      let ff , lit = make_form acc f.c f.annot in
      F.mk_not ff , lit
    | TFop(OPif t,[f2;f3]) ->
      let tt = make_term t in
      let ff2 , lit2 = make_form acc f2.c f2.annot in
      let ff3 , lit3 = make_form lit2 f3.c f3.annot in
      F.mk_if tt ff2 ff3 id, lit3
    | TFop(OPiff,[f1;f2]) ->
      let ff1 , lit1 = make_form acc f1.c f1.annot in
      let ff2 , lit2 = make_form lit1 f2.c f2.annot in
      F.mk_iff ff1 ff2 id, lit2
    | (TFforall qf | TFexists qf) as f ->
      let name =
        if !name_tag = 0 then name_base else
          sprintf "#%s#sub-%d" name_base !name_tag
      in
      incr name_tag;
      let qvars = varset_of_list qf.qf_bvars in
      let binders = F.mk_binders qvars in
      (*let upvars = varset_of_list qf.qf_upvars in*)
      let trs = List.map make_trigger qf.qf_triggers in
      let ff , lit = make_form acc qf.qf_form.c qf.qf_form.annot in
      let func = match f with
	| TFforall _ -> F.mk_forall
        | TFexists _ -> F.mk_exists
        | _ -> assert false
      in
      func name loc binders trs ff id None, lit

    | TFlet(up,lvar,lterm,lf) ->
      let ff, lit = make_form acc lf.c lf.annot in
      F.mk_let (varset_of_list up) lvar (make_term lterm) ff id, lit

    | TFnamed(lbl, f) ->
      let ff, lit = make_form acc f.c f.annot in
      F.add_label lbl ff;
      ff, lit

    | _ -> assert false
  in
  make_form [] f.c f.annot

let push_assume queue f name loc match_flag =
  let ff , _ = make_form name f loc in
  Queue.push {st_decl=Assume(ff, match_flag) ; st_loc=loc} queue

let push_preddef queue f name loc match_flag =
  let ff , _ = make_form name f loc  in
  Queue.push {st_decl=PredDef (ff, name) ; st_loc=loc} queue

let push_query queue n f loc sort =
  let ff, lits = make_form "" f loc in
  Queue.push {st_decl=Query(n, ff, lits, sort) ; st_loc=loc} queue

let make_rule ({rwt_left = t1; rwt_right = t2} as r) =
  { r with rwt_left = make_term t1; rwt_right = make_term t2 }

let make l =
  let queue = Queue.create () in
  List.iter
    (fun (d,b) -> match d.c with
    | TAxiom(loc, name, f) -> push_assume queue f name loc b
    | TRewriting(loc, name, lr) ->
      Queue.push
	{st_decl=RwtDef(List.map make_rule lr); st_loc=loc} queue
    | TGoal(loc, sort, n, f) -> push_query queue n f loc sort
    (*| TPredicate_def(loc, n, [], f) -> push_preddef queue f n loc b*)
    | TPredicate_def(loc, n, _, f) -> push_preddef queue f n loc b
    | TFunction_def(loc, n, _, _, f) -> push_assume queue f n loc b
    | TTypeDecl _ | TLogic _  -> ()) l;
  queue
