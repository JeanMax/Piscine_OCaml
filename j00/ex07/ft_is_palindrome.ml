(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/01 22:11:33 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome (s) =
	let len = String.length s
	in let rec check i = if i <= len / 2 - 1 then 
						   s.[i] = s.[len - i - 1] && check(i + 1) else true
	 in check(0)

let() =
	print_endline "Testing 'radar':";
	print_endline (if ft_is_palindrome("radar") then "true" else "false");
	print_endline "\nTesting 'madam':";
	print_endline (if ft_is_palindrome("madam") then "true" else "false");
	print_endline "\nTesting 'car':";
	print_endline (if ft_is_palindrome("car") then "true" else "false");
	print_endline "\nTesting '':";
	print_endline (if ft_is_palindrome("") then "true" else "false")
