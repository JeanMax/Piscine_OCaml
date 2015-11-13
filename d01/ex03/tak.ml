(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 00:42:07 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 00:48:31 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec tak x y z =
  if y >= x then z
  else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

let() =
  print_endline "testing 1 2 3: "; print_int (tak 1 2 3);
  print_endline "\ntesting 5 23 7: "; print_int (tak 5 23 7);
  print_endline "\ntesting 9 1 0: "; print_int (tak 9 1 0);
  print_endline "\ntesting 1 1 1: "; print_int (tak 1 1 1);
  print_endline "\ntesting 0 42 0: "; print_int (tak 0 42 0);
  print_endline "\ntesting 23498 98734 98776: "; print_int (tak 23498 98734 98776);
  print_endline ""
