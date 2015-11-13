(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/02 01:02:22 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
	let rec print (x, y, z) =
	print_int x;
	print_int y;
	print_int z;
	match (x, y, z) with
		(7, 8, 9) -> print_string "\n"
	 |	(_, 8, 9) -> print_string ", "; print((x + 1), (x + 2), (x + 3))
	 |	(_, _, 9) -> print_string ", "; print( x,	   (y + 1), (y + 2))
	 |	(_, _, _) -> print_string ", "; print( x,		y,		(z + 1))
	in print(0, 1, 2)

let() =
	ft_print_comb()
