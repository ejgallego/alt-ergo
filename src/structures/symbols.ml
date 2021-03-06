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

open Hashcons

type operator =
  | Plus | Minus | Mult | Div | Modulo | Concat | Extract
  | Get | Set | Access of Hstring.t | Record

type name_kind = Ac | Constructor | Other

type t =
  | True
  | False
  | Void
  | Name of Hstring.t * name_kind
  | Int of Hstring.t
  | Real of Hstring.t
  | Bitv of string
  | Op of operator
  | Var of Hstring.t
type s = t

let name ?(kind=Other) s = Name (Hstring.make s, kind)
let var s = Var (Hstring.make s)
let int i = Int (Hstring.make i)
let real r = Real (Hstring.make r)

let is_ac = function
  | Name(_, Ac) -> true
  | _           -> false

let underscoring = function
Var s -> Var (Hstring.make ("$"^Hstring.view s))
  | _ -> assert false

let compare_kind k1 k2 = match k1, k2 with
  | Ac   , Ac    -> 0
  | Ac   , _     -> 1
  | _    , Ac    -> -1
  | Other, Other -> 0
  | Other, _     -> 1
  | _    , Other -> -1
  | Constructor, Constructor -> 0

let compare s1 s2 =  match s1, s2 with
  | Name (n1,k1), Name (n2,k2) ->
    let c = compare_kind k1 k2 in
    if c = 0 then Hstring.compare n1 n2 else c
  | Name _, _ ->  -1
  | _, Name _ -> 1
  | Var n1, Var n2 -> Hstring.compare n1 n2
  | Var _, _ -> -1
  | _ ,Var _ -> 1
  | Int i1, Int i2 -> Hstring.compare i1 i2
  | Int _, _ -> -1
  | _ ,Int _ -> 1
  | Op(Access s1), Op(Access s2) -> Hstring.compare s1 s2
  | Op(Access _), _ -> -1
  | _, Op(Access _) -> 1
  | _  -> Pervasives.compare s1 s2

let equal s1 s2 = compare s1 s2 = 0

let hash = function
  | Name (n,Ac) -> Hstring.hash n * 19 + 1
  | Name (n,_) -> Hstring.hash n * 19
  | Var n (*| Int n*) -> Hstring.hash n * 19 + 1
  | Op (Access s) -> Hstring.hash s + 19
  | s -> Hashtbl.hash s

let to_string =  function
  | Name (n,_) -> Hstring.view n
  | Var x -> Format.sprintf "'%s'" (Hstring.view x)
  | Int n -> Hstring.view n
  | Real n -> Hstring.view n
  | Bitv s -> "[|"^s^"|]"
  | Op Plus -> "+"
  | Op Minus -> "-"
  | Op Mult -> "*"
  | Op Div -> "/"
  | Op Modulo -> "%"
  | Op (Access s) -> "@Access_"^(Hstring.view s)
  | Op Record -> "@Record"
  | Op Get -> "get"
  | Op Set -> "set"
  | True -> "true"
  | False -> "false"
  | Void -> "void"
  | _ -> "" (*assert false*)

let print fmt s = Format.fprintf fmt "%s" (to_string s)

let dummy = Name (Hstring.make "_one", Other)

let fresh =
  let cpt = ref 0 in
  fun s ->
    incr cpt;
    (* garder le suffixe "__" car cela influence l'ordre *)
    name (Format.sprintf "!?__%s%i" s (!cpt))

let is_get f = equal f (Op Get)
let is_set f = equal f (Op Set)

let fake_eq  =  name "@eq"
let fake_neq =  name "@neq"
let fake_lt  =  name "@lt"
let fake_le  =  name "@le"

module Map =
  Map.Make(struct type t' = t type t=t' let compare=compare end)

module Set =
  Set.Make(struct type t' = t type t=t' let compare=compare end)



module Labels = Hashtbl.Make(struct
  type t = s
  let equal = equal
  let hash = hash
end)

let labels = Labels.create 100007

let add_label lbl t = Labels.replace labels t lbl

let label t = try Labels.find labels t with Not_found -> Hstring.empty
