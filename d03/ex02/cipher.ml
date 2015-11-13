(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 14:29:59 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 16:32:34 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec caesar n s =
  if n < 0 then caesar (n + 128) s else
	let rot c =
	  char_of_int ((int_of_char c + n) mod 128)
	in String.map rot s

let rot42 s =
  caesar 42 s

let xor n s =
  String.map (fun c -> char_of_int (n lxor (int_of_char c))) s


let rec ft_crypt (s:string) = function
	[]         -> s
  | head::tail -> ft_crypt (head s) tail
