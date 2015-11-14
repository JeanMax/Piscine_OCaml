(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Calc.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/13 21:56:18 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 00:35:22 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type MONOID =
  sig
	type element
    val zero1 : element
    val zero2 : element
    val mul : element -> element -> element
    val add : element -> element -> element
    val div : element -> element -> element
    val sub : element -> element -> element
  end

(*
module type CALC =
  sig
	val add : M.element -> M.element -> M.element
	val sub : M.element -> M.element -> M.element
	val mul : M.element -> M.element -> M.element
	val div : M.element -> M.element -> M.element
	val power : M.element -> int -> M.element
	val fact : M.element -> M.element
  end


module type MAKECALC = 
  functor (M : MONOID) -> CALC with type element = M.element
 *)

(* ************************************************************************** *)


module INT : (MONOID with type element = int) =
  struct
	type element = int
    let zero1 = 0
    let zero2 = 1
    let mul = ( * )
    let add = ( + )
    let div = ( / )
    let sub = ( - )
  end


module FLOAT : (MONOID with type element = float) =
  struct
	type element = float
    let zero1 = 0.
    let zero2 = 1.
    let mul = ( *. )
    let add = ( +. )
    let div = ( /. )
    let sub = ( -. )
  end


module Calc (*: MAKECALC*) =
  functor (M : MONOID) ->
    struct
	  let add = M.add
	  let sub = M.sub
	  let mul = M.mul
	  let div = M.div

	  (* x^n = if !(n % 2) then (x^2)^(n/2) else x*(x^2)^((n-1)/2) *)
	  let power x n = 
		let rec zboub a b = function
			z when z < 0       -> zboub a (M.div M.zero2 b) (-z)
		  | 0                  -> a
		  | 1                  -> M.mul a b 
		  | z when z mod 2 = 0 -> zboub a (M.mul b b) (z/2)
		  | z                  -> zboub (M.mul a b) (M.mul b b) ((z-1)/2)
		in zboub M.zero2 x n

	  let fact n =
		let rec zboub i acc =
		  if i > n then acc
		  else zboub (M.add i M.zero2) (M.mul acc i)
		in zboub M.zero2 M.zero2
	end


(* ************************************************************************** *)


module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
  print_endline "testing power 3 3:";
  print_endline (string_of_int (Calc_int.power 3 3));

  print_endline "testing power 3. 3:";
  print_endline (string_of_float (Calc_float.power 3. 3));

  print_endline "testing mul (add 20 1) 2:";
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));

  print_endline "testing mul (add 20. 1.) 2.:";
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20. 1.) 2.));

  print_endline "testing sub 84 42:";
  print_endline (string_of_int (Calc_int.sub 84 42));

  print_endline "testing sub 84. 42.:";
  print_endline (string_of_float (Calc_float.sub 84. 42.));

  print_endline "testing div 84 2:";
  print_endline (string_of_int (Calc_int.div 84 2));

  print_endline "testing div 84. 2.:";
  print_endline (string_of_float (Calc_float.div 84. 2.));

  print_endline "testing sub (add 42 zero1) zero1:";
  print_endline (string_of_int (Calc_int.sub (Calc_int.add 42 INT.zero1) INT.zero1));

  print_endline "testing sub (add 42. zero1) zero1:";
  print_endline (string_of_float (Calc_float.sub (Calc_float.add 42. FLOAT.zero1) FLOAT.zero1));

  print_endline "testing mul (div 42 zero2) zero2:";
  print_endline (string_of_int (Calc_int.mul (Calc_int.div 42 INT.zero2) INT.zero2));

  print_endline "testing mul (div 42. zero2) zero2:";
  print_endline (string_of_float (Calc_float.mul (Calc_float.div 42. FLOAT.zero2) FLOAT.zero2));

  print_endline "testing fact 3:";
  print_endline (string_of_int (Calc_int.fact 3));

  print_endline "testing fact 3.:";
  print_endline (string_of_float (Calc_float.fact 3.));

  print_endline "testing fact 142.:";
  print_endline (string_of_float (Calc_float.fact 142.));

  print_endline "testing power 142. 142:";
  print_endline (string_of_float (Calc_float.power 142. 142));

