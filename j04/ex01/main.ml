(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 19:15:39 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 20:22:13 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let() =
  print_endline "testing toString/toStringVerbose on all:";
  let rec print = function
	  []         -> ()
	| head::tail -> print_endline (
						(Value.toString head) ^ "/" ^ (Value.toStringVerbose head) );
					print tail
  in print Value.all;
	 print_endline "\ntesting next:";
	 let rec next v =
	   print_endline (Value.toString v);
	   try next (Value.next v) with invalid_arg -> print_endline "invalid arg caught"
	 in next Value.T2;
		print_endline "\ntesting previous:";
		let rec previous v =
		  print_endline (Value.toString v);
		  try previous (Value.previous v) with invalid_arg -> print_endline "invalid arg caught"
		in previous Value.As
	 
