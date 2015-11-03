(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:18:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 17:24:31 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () =
	let rec zboub i =
	  print_char (char_of_int i);
	  if i < 122 then zboub (i + 1) else print_char '\n'
	in zboub 97

let() =
	ft_print_alphabet()
