(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/08 02:51:42 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_string_all f s =
  let rec check i =
	if i >= 0 then f(s.[i]) && check(i - 1) else true
  in check(String.length s - 1)

let is_digit c = c >= '0' && c <= '9'

let() =
	print_endline "Testing is_digit '1234':";
	print_endline (if ft_string_all is_digit "1234" then "true" else "false");
	print_endline "\nTesting is_digit 'E1234':";
	print_endline (if ft_string_all is_digit "E1234" then "true" else "false");
	print_endline "\nTesting is_digit '':";
	print_endline (if ft_string_all is_digit "" then "true" else "false")
