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

type answer = Yes of Explanation.t * Term.Set.t list | No

type 'a ac = {h: Symbols.t ; t: Ty.t ; l: ('a * int) list}

type 'a literal = LSem of 'a Literal.view | LTerm of Literal.LT.t
                  | LBoxed of Boxed.t

type 'a input =  
    'a Literal.view * Literal.LT.t option * Explanation.t

type 'a result = { 
  assume : ('a literal * Explanation.t) list;  
  remove: ('a literal * Explanation.t) list;
}

type 'a solve_pb = { sbt : ('a * 'a) list; eqs : ('a * 'a) list }

module type RELATION = sig
  type t
  type r

  val empty : Term.Set.t list -> t
    
  val assume : 
    t -> 
    (r input) list -> 
    are_eq : (Term.t -> Term.t -> answer) -> 
    are_neq : (Term.t -> Term.t -> answer) -> 
    class_of : (Term.t -> Term.t list) -> 
    classes : Term.Set.t list -> 
    t * r result

  val query : 
    t -> 
    r input -> 
    are_eq : (Term.t -> Term.t -> answer) -> 
    are_neq : (Term.t -> Term.t -> answer) -> 
    class_of : (Term.t -> Term.t list) ->  
    classes : Term.Set.t list -> 
    answer

  val case_split : t -> (r Literal.view * Explanation.t * Numbers.Q.t) list
  (** case_split env returns a list of equalities *)
    
  val add : t -> r -> t
  (** add a representant to take into account *)

  val print_model : Format.formatter -> t -> (Term.t * r) list -> unit
    
  val new_terms : t -> Term.Set.t


end

module type THEORY = sig

  (**Type of terms of the theory*)
  type t
  (**Type of representants of terms of the theory*)
  type r
  (** Name of the theory*)
  val name : string
  (** return true if the symbol is owned by the theory*)
  val is_mine_symb : Symbols.t -> bool

  (** Give a representant of a term of the theory*)
  val make : Term.t -> r * Literal.LT.t list

  val term_extract : r -> Term.t option

  val color : (r ac) -> r
    
  val type_info : t -> Ty.t
    
  val embed : r -> t

  (** Give the leaves of a term of the theory *)
  val leaves : t -> r list
  val subst : r -> r -> t -> r

  val compare : r -> r -> int

  val hash : t -> int
  (** solve r1 r2, solve the equality r1=r2 and return the substitution *)

  val solve : r -> r ->  r solve_pb -> r solve_pb

  val print : Format.formatter -> t -> unit

  val fully_interpreted : Symbols.t -> bool

  val abstract_selectors : t -> (r * r) list -> r * (r * r) list

  module Rel : RELATION with type r = r
end

module type X = sig
  type r

  val make : Term.t -> r * Literal.LT.t list
    
  val type_info : r -> Ty.t
    
  val compare : r -> r -> int
    
  val equal : r -> r -> bool

  val hash : r -> int
    
  val leaves : r -> r list
    
  val subst : r -> r -> r -> r
    
  val solve : r -> r ->  (r * r) list
    
  val term_embed : Term.t -> r

  val term_extract : r -> Term.t option 

  val ac_embed : r ac -> r
    
  val ac_extract : r -> (r ac) option
    
  val color : (r ac) -> r

  val fully_interpreted : Symbols.t -> bool
    
  val print : Format.formatter -> r -> unit
    
  val abstract_selectors : r -> (r * r) list -> r * (r * r) list
    
  module Rel : RELATION with type r = r

end

module type C = sig
  type t
  type r
  val extract : r -> t option
  val embed : t -> r
end

module type CC =  sig

  module Rel : sig 
    type t
    type r
    type choice

    val choice_to_literal : choice -> r literal
    val choice_mk_not : choice -> choice
    val choice_print : Format.formatter -> choice -> unit
    val extract_terms_from_choice : Term.Set.t -> choice -> Term.Set.t

    val query : t -> r input ->
      are_eq:(Term.t -> Term.t -> answer) ->
      are_neq:(Term.t -> Term.t -> answer) ->
      class_of:(Term.t -> Term.t list) ->
      classes:Term.Set.t list -> answer

    val case_split : t -> (choice * Explanation.t * Numbers.Q.t) list

    val print_model : Format.formatter -> t -> (Term.t * r) list -> unit

    val new_terms : t -> Term.Set.t
  end

  type use
  type uf
  type 'a accumulator

  type env = { 
    use : use;  
    uf : uf ;
    relation : Rel.t }

  val empty : unit -> env
  val assume_literal : env -> Rel.r accumulator ->
    (Rel.r literal * Explanation.t) list -> env * Rel.r accumulator
  val add : env -> Rel.r accumulator -> Literal.LT.t -> 
    Explanation.t -> env * Rel.r accumulator
  val term_canonical_view : env -> Literal.LT.t -> Explanation.t ->
    (Rel.r Literal.view * Explanation.t)

end
