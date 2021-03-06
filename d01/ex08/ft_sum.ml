(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 01:49:27 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 19:21:25 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f start stop =
  if stop < start then nan else
	let rec zboub n acc =
	  if n > stop then acc else zboub (n + 1) (acc +. f n)
	in zboub start 0.0

let() =
	print_endline "testing (fun i -> float_of_int (i * i)) 1 10: ";
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_endline "\ntesting (fun i -> float_of_int (i * i)) 10 1: ";
	print_float (ft_sum (fun i -> float_of_int (i * i)) 10 1);
	print_endline "\ntesting (fun i -> float_of_int (i * i)) 10 10: ";
	print_float (ft_sum (fun i -> float_of_int (i * i)) 10 10);
	print_endline ""
