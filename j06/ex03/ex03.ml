(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/10 21:44:32 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 01:38:38 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FIXED =
sig
    type t
    val of_float  : float -> t
    val of_int    : int -> t
    val to_float  : t -> float
    val to_int    : t -> int
    val to_string : t -> string
    val zero : t
    val one  : t
    val succ : t -> t
    val pred : t -> t
    val min  : t -> t -> t
    val max  : t -> t -> t
    val gth  : t -> t -> bool
    val lth  : t -> t -> bool
    val gte  : t -> t -> bool
    val lte  : t -> t -> bool
    val eqp  : t -> t -> bool (** physical equality *)
    val eqs  : t -> t -> bool (** structural equality *)
    val add  : t -> t -> t
    val sub  : t -> t -> t
    val mul  : t -> t -> t
    val div  : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
end

(******************************************************************************)

module type FRACTIONNAL_BITS = 
  sig 
	val bits : int 
  end

module type MAKE = 
  functor (Fb:FRACTIONNAL_BITS) -> FIXED

module Make : MAKE = 
  functor (Fb:FRACTIONNAL_BITS) ->
  struct
	type t = int
    let of_float f = int_of_float 
					   (floor (f *. float_of_int (1 lsl Fb.bits) +. 0.5))
    let of_int i = i lsl Fb.bits
    let to_float fb = float_of_int fb /. float_of_int (1 lsl Fb.bits)
    let to_int fb = fb lsr Fb.bits
    let to_string fb = string_of_float (to_float fb)
    let zero = 0
    let one  = 1 lsl Fb.bits
    let succ fb = fb + 1
    let pred fb = fb - 1
    let min fb1 fb2 = if fb1 <= fb2 then fb1 else fb2
    let max fb1 fb2 = if fb1 >= fb2 then fb1 else fb2
    let gth fb1 fb2 = fb1 > fb2
    let lth fb1 fb2 = fb1 < fb2
    let gte fb1 fb2 = fb1 >= fb2
    let lte fb1 fb2 = fb1 <= fb2
    let eqp fb1 fb2 = fb1 == fb2
    let eqs fb1 fb2 = fb1 = fb2
    let add fb1 fb2 = fb1 + fb2
    let sub fb1 fb2 = fb1 - fb2
    let mul fb1 fb2 = (fb1 * fb2 + (1 lsl Fb.bits - 1)) lsr (1 lsl Fb.bits)
    let div fb1 fb2 = 
	  let tmp = (fb1 lsl Fb.bits) in
	  if (tmp >= 0 && fb2 >= 0) || (tmp < 0 && fb2 < 0)
	  then (tmp + fb2 / 2) / fb2 else (tmp - fb2 / 2) / fb2
    let foreach fb1 fb2 f = 
	  let rec zboub x = 
		if x <= fb2 then (f x; zboub (x + 1))
	  in zboub fb1
  end

let test () =
  () (* TODO *)

(******************************************************************************)

module Fixed4 : FIXED = 
  Make (struct let bits = 4 end)

module Fixed8 : FIXED = 
  Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) 
				   (Fixed4.one) 
				   (fun f -> print_endline (Fixed4.to_string f));
	test ()
