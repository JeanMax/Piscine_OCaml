(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 01:25:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 01:55:14 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
  if n < 0 then (-1) else
	let rec zboub m acc =
	  if (m = 0) then acc else zboub (m - 1) (f acc)
	in zboub n x

let() =
  print_endline "testing x² 2 -42: "; print_int (iter (fun x -> x * x) 2 (-42));
  print_endline "\ntesting x² 2 4: "; print_int (iter (fun x -> x * x) 2 4);
  print_endline "\ntesting 2x 2 4: "; print_int (iter (fun x -> x * 2) 2 4);
  print_endline ""
