(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 18:58:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/02 19:58:12 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x i =
  let rec zboub(x, str) =
	match x with
	  0 -> str
	| z when (z > 0) -> zboub(x - 1, str ^ "x")
	| _ -> "Error"
	in zboub(i, "")

let() =
	print_endline "testing -1: ";
	print_endline (repeat_x(-1));
	print_endline "testing 0: ";
	print_endline (repeat_x 0);
	print_endline "testing 1: ";
	print_endline (repeat_x 1);
	print_endline "testing 5: ";
	print_endline (repeat_x 5)
