(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 02:02:46 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 03:08:56 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi d =
  if d < 0. then (-1) else
	let rec zboub i f acc =
	  let diff = if atan 1. -. acc > 0. 
				 then atan 1. -. acc else acc -. atan 1. in
(*
	  print_string "pi: "; print_float (4. *. acc); print_endline "";
	  print_string "diff: "; print_float (diff); print_endline ""; 
 *)
	   if diff < d then i else
		zboub (i + 1) (f +. 1.) (acc +. (((-1.) ** f) /. (2. *. f +. 1.)))
	in zboub 0 0. 0.

let() =
	print_endline "testing delta = -42.: ";
	print_int (leibniz_pi (-42.));
	print_endline "\ntesting delta = 0.1: ";
	print_int (leibniz_pi 0.1);
	print_endline "\ntesting delta = 0.01: ";
	print_int (leibniz_pi 0.01);
	print_endline "\ntesting delta = 0.001: ";
	print_int (leibniz_pi 0.001);
	print_endline ""
