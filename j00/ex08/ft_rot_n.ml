(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/02 00:35:22 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n (n, str) =
	let rot(c) =
	match c with
		'a'..'z' -> char_of_int ((int_of_char c + n - 97) mod 26 + 97)
	 |	'A'..'Z' -> char_of_int ((int_of_char c + n - 65) mod 26 + 65)
	 |	_		 -> c
	in String.map rot str

let() =
	print_endline(ft_rot_n(1, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
