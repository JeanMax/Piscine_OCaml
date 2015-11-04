(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/02 18:58:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 16:52:39 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x i =
  let rec zboub x str =
	if x = 0 then str
	else if x > 0 then zboub (x - 1) (str ^ "x")
	else "Error"
	in zboub i ""

let() =
	print_endline "testing -1: ";
	print_endline (repeat_x(-1));
	print_endline "testing 0: ";
	print_endline (repeat_x 0);
	print_endline "testing 1: ";
	print_endline (repeat_x 1);
	print_endline "testing 5: ";
	print_endline (repeat_x 5)
