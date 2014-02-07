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

module F = Formula

type e = Dep of Formula.t | BJ of Formula.t | Fresh of int

module ES = Set.Make(struct 
  type t = e 
  let compare e1 e2 = match e1,e2 with
    | Dep e1, Dep e2 -> Formula.compare e1 e2
    | Dep _, _ -> 1
    | _, Dep _ -> -1
    | BJ e1, BJ e2 -> Formula.compare e1 e2
    | BJ _, _ -> 1
    | _, BJ _ -> -1
    | Fresh i1, Fresh i2 -> Pervasives.compare i1 i2
end)

type t = ES.t

let empty = ES.empty

let mem_as_bj l s = ES.mem (BJ l) s

let singleton ?(bj = true) l = 
  ES.singleton (if bj then BJ l else Dep l)

let make_deps sf = 
  Formula.Set.fold (fun l acc -> ES.add (BJ l) acc) sf ES.empty

let union s1 s2 = ES.union s1 s2


(* TODO : XXX : We have to choose the smallest ??? *)
let merge d1 d2 = d1

let remove f s =
  if not (ES.mem (BJ f) s) then raise Not_found;
  ES.remove (BJ f) s

let print fmt s = 
  Format.fprintf fmt "[";
  ES.iter (fun e -> match e with 
    | Dep f -> Format.fprintf fmt "{Dep:%a}" Formula.print f
    | BJ f -> Format.fprintf fmt "{BJ:%a}" Formula.print f
    | Fresh i -> Format.fprintf fmt "{Fresh:%i}" i;
  ) s;
  Format.fprintf fmt "]"

let print_proof fmt s =
  ES.iter (fun e -> match e with 
    | (Dep f | BJ f) -> Format.fprintf fmt "  %a@." Formula.print f
    | Fresh i -> ()
  (* | BJ f  -> Format.fprintf fmt "  %a@." Formula.print f *)
  ) s

let formulas_of s =
  ES.fold (fun e acc -> 
    match e with 
      | Dep f | BJ f -> F.Set.add f acc
      | Fresh _ -> acc
  ) s F.Set.empty


let rec literals_of_acc lit fs f acc = match F.view f with
  | F.Literal _ ->
    if lit then f :: acc else acc
  | F.Unit (f1,f2) ->
    let acc = literals_of_acc false fs f1 acc in
    literals_of_acc false fs f2 acc
  | F.Clause (f1, f2) -> 
    let acc = literals_of_acc true fs f1 acc in
    literals_of_acc true fs f2 acc
  | F.Lemma _ ->
    acc
  | F.Skolem {F.sko_f = f1} | F.Let {F.let_f = f1} ->
    literals_of_acc true fs f1 acc

let literals_of ex =
  let fs  = formulas_of ex in   
  F.Set.fold (literals_of_acc true fs) fs []


module MI = Map.Make (struct type t = int let compare = compare end)

let literals_ids_of ex =
  List.fold_left (fun acc f ->
    let i = F.id f in
    let m = try MI.find i acc with Not_found -> 0 in
    MI.add i (m + 1) acc
  ) MI.empty (literals_of ex)


type exp = int

let fresh_exp =
  let r = ref (-1) in
  fun () -> incr r; !r

let remove_fresh i s =
  if ES.mem (Fresh i) s then Some (ES.remove (Fresh i) s)
  else None

let add_fresh i s = ES.add (Fresh i) s
