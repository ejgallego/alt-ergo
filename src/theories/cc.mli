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

(* Persistent and incremental congruence-closure modulo X data structure. 

   This module implements the CC(X) algorithm.

*)

module type S = sig
  type t

  val empty : unit -> t
  val assume : Literal.LT.t -> Explanation.t -> t -> t * Term.Set.t * int
  val query : Literal.LT.t -> t -> Sig.answer
  val class_of : t -> Term.t -> Term.t list
  val print_model : Format.formatter -> t -> unit
  val cl_extract : t -> Term.Set.t list
  val term_repr : t -> Term.t -> Term.t
end

module Make (X:Sig.X) : S