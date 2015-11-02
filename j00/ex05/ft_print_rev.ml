(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/01 21:45:47 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev (str) =
	for i = String.length str - 1 downto 0 do
	print_char str.[i]
	done;
	print_char '\n'

let() =
	ft_print_rev "Hello world !";
	ft_print_rev ""
