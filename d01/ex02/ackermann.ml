(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 00:03:02 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 16:58:54 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ackermann x y =
	if x < 0 || y < 0 then (-1) else
	  let rec zboub m n =
		if m = 0 then n + 1
		else if n = 0 then zboub (m - 1) 1
		else zboub (m - 1) (zboub m (n - 1))
	  in zboub x y

let() =
	print_endline "testing -1 7: "; print_int (ackermann (-1) 7);
	print_endline "\ntesting 0 0: "; print_int (ackermann 0 0);
	print_endline "\ntesting 2 3: "; print_int (ackermann 2 3);
	print_endline "\ntesting 4 1: "; print_int (ackermann 4 1);
	print_endline ""
