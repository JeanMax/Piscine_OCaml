(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/02 01:18:41 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb2 () =
	let rec print (x, y) =
	if (x < 10) then print_int 0;
	print_int x;
	print_char ' ';
	if (y < 10) then print_int 0;
	print_int y;
	match (x, y) with
		(98, 99) -> print_char '\n'
	 |	( _, 99) -> print_char ','; print_char ' '; print((x + 1),	(x + 2))
	 |	( _,  _) -> print_char ','; print_char ' '; print( x,		(y + 1))
	in print(0, 1)

let() =
	ft_print_comb2()
