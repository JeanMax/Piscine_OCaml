(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 19:44:45 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 17:41:18 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x =
	if x <= 0 then (print_int 0; print_char '\n')
	else (print_int x; print_char '\n'; ft_countdown (x - 1))

let() =
	print_endline "testing 5:"; ft_countdown 5; print_char '\n';
	print_endline "testing -5:"; ft_countdown (-5); print_char '\n';
	print_endline "testing 0:"; ft_countdown 0
