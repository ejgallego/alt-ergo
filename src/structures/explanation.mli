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

type t
(** An explanation set *)

(* val everything : t *)

val empty : t

val mem_as_bj : Formula.t -> t -> bool

val singleton : ?bj:bool -> Formula.t -> t

val make_deps : Formula.Set.t -> t

val union : t -> t -> t

val merge : t -> t -> t

val remove : Formula.t -> t -> t

val print : Format.formatter -> t -> unit

val print_proof : Format.formatter -> t -> unit

val formulas_of : t -> Formula.Set.t

module MI : Map.S with type key = int

val literals_ids_of : t -> int MI.t


(** Fresh *)

type exp

val fresh_exp : unit -> exp
(** create a fresh explanation *)
val remove_fresh : exp -> t -> t option
(** try to remove a fresh explanation. Return None if the given explanation
    is not in the set  *)
val add_fresh : exp -> t -> t
(** Add a fresh explanation to an explanation set *)