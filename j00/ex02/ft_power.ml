(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 19:59:14 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 17:42:01 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x y =
	if y = 0 then 1 else x * ft_power x (y - 1)

let() =
	print_endline "testing 2 4:"; print_int(ft_power 2 4);
	print_endline "\ntesting 3 0:"; print_int(ft_power 3 0);
	print_endline "\ntesting 0 5:"; print_int(ft_power 0 5);
	print_endline ""
