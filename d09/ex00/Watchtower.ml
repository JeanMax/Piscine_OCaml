(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/13 20:49:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 22:24:10 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type WATCHTOWER =
  sig
	type hour = int
	val zero : hour
	val add  : hour -> hour -> hour
	val sub  : hour -> hour -> hour
  end


(* ************************************************************************** *)


module Watchtower : (WATCHTOWER with type hour = int) =
  struct
	type hour = int
	let zero = 12
	let rec add x y = if x < 0 || y < 0
					  then add (x + 12) (y + 12)
					  else (x + y) mod zero
	let sub x y = add x (-y)
  end


(* ************************************************************************** *)


let () =
  print_endline "testing zero:";
  print_endline (string_of_int Watchtower.zero);

  print_endline "\ntesting 1 + 1:";
  print_endline (string_of_int (Watchtower.add 1 1));

  print_endline "\ntesting 1 + 12:";
  print_endline (string_of_int (Watchtower.add 1 12));

  print_endline "\ntesting -42 + 12:";
  print_endline (string_of_int (Watchtower.add (-42) 12));

  print_endline "\ntesting 1 - 1:";
  print_endline (string_of_int (Watchtower.sub 1 1));

  print_endline "\ntesting 1 - 12:";
  print_endline (string_of_int (Watchtower.sub 1 12));

  print_endline "\ntesting -42 - 12:";
  print_endline (string_of_int (Watchtower.sub (-42) 12));

