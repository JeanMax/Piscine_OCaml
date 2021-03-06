(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 19:26:30 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 16:55:18 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?(str="x") n =
  let rec zboub x s str =
	if x = 0 then s
	else if x > 0 then zboub (x - 1) (s ^ str) str
	else "Error"
  in zboub n "" str

let() =
	print_endline "testing -1: ";
	print_endline (repeat_string(-1));
	print_endline "testing 0: ";
	print_endline (repeat_string 0);
	print_endline "testing 5: ";
	print_endline (repeat_string 5);
	print_endline "testing toto -1: ";
	print_endline (repeat_string ~str:"toto" (-1));
	print_endline "testing toto 0: ";
	print_endline (repeat_string ~str:"toto" 0);
	print_endline "testing toto 5: ";
	print_endline (repeat_string ~str:"toto" 5)
