(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 19:05:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/02 19:47:56 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign (x) =
	print_endline (if x < 0 then "negative" else "positive")

let() = 
	print_endline "Testing from -1 to 1:";
	for i = -1 to 1 do
	  ft_test_sign (i)
	done
