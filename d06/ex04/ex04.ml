(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex04.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/10 23:59:36 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 01:37:32 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type VAL =
  sig
    type t
    val add : t -> t -> t
    val mul : t -> t -> t
  end
	
(******************************************************************************)

module type EVALEXPR =
  sig
    type t
    type expr = Add of expr * expr | Mul of expr * expr | Value of t

    val eval : expr -> t
  end

module type MAKEEVALEXPR = 
  functor (Ee:VAL) -> EVALEXPR with type t = Ee.t

module MakeEvalExpr : MAKEEVALEXPR = 
  functor (Ee:VAL) ->
  struct
	type t = Ee.t
	type expr = Add of expr * expr | Mul of expr * expr | Value of t

	let rec eval = function
		Add (x, y) -> Ee.add (eval x) (eval y)
	  | Mul (x, y) -> Ee.mul (eval x) (eval y)
	  | Value (v)  -> v
  end						 

(******************************************************************************)

module IntVal : (VAL with type t = int) =
  struct
    type t = int
    let add = ( + )
    let mul = ( * )
  end
	
module FloatVal : (VAL with type t = float) =
  struct
    type t = float
    let add = ( +. )
    let mul = ( *. )
  end

module StringVal : (VAL with type t = string) =
  struct
    type t = string
    let add s1 s2 = if (String.length s1) > (String.length s2) then s1 else s2
    let mul = ( ^ )
  end

module IntEvalExpr    : (EVALEXPR with type t := IntVal.t) = 
  MakeEvalExpr (IntVal)

module FloatEvalExpr  : (EVALEXPR with type t := FloatVal.t) =
  MakeEvalExpr (FloatVal)

module StringEvalExpr : (EVALEXPR with type t := StringVal.t) = 
  MakeEvalExpr (StringVal)

let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)
let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ",
                             StringEvalExpr.Add 
							   (StringEvalExpr.Value "very long",
                                StringEvalExpr.Value "short"))

let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe)
let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se)
