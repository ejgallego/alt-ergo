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

type +'a t = 'a Symbols.Map.t

module type PRINT_TYPE = sig
  type t
  val print : Format.formatter -> t -> unit
end

module type S = sig
  type elt

  include Map.S with type key = Symbols.t and type 'a t = 'a t

  val print : Format.formatter -> elt t -> unit
end



module Make(X : PRINT_TYPE) = struct
  include Symbols.Map 

  type elt  = X.t

  let print fmt = 
    iter (fun k v -> fprintf fmt "%a -> %a  " Symbols.print k X.print v)

end

