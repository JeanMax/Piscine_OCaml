(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 19:02:20 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 19:10:17 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let() =
  print_endline "testing toString/toStringVerbose on all:";
  let rec print = function
	  []         -> ()
	| head::tail -> print_endline (
						(Color.toString head) ^ "/" ^ (Color.toStringVerbose head) );
					print tail
  in print Color.all;	 
