(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 00:48:59 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 01:18:33 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
  if n < 0 then (-1) else
  let rec zboub n x y = 
	if n = 0 then x
	else zboub (n - 1) y (x + y)
  in zboub n 0 1

let() =
  print_endline "testing -42: "; print_int (fibonacci (-42));
  print_endline "\ntesting 0: "; print_int (fibonacci 0);
  print_endline "\ntesting 1: "; print_int (fibonacci 1);
  print_endline "\ntesting 2: "; print_int (fibonacci 2);
  print_endline "\ntesting 3: "; print_int (fibonacci 3);
  print_endline "\ntesting 4: "; print_int (fibonacci 4);
  print_endline "\ntesting 5: "; print_int (fibonacci 5);
  print_endline "\ntesting 6: "; print_int (fibonacci 6);
