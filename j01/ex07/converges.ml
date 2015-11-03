(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 01:37:47 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 01:48:26 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let converges f x n =
	let rec zboub m acc =
	if (m < 0) then false 
	else if (acc = f acc) then true 
	else zboub (m - 1) (f acc)
	in zboub n x

let() =
	print_endline "testing (( * ) 2) 2 5:";
	print_endline (if converges (( * ) 2) 2 5 then "true" else "false");
	print_endline "testing (fun x -> x / 2) 2 3:";
	print_endline (if converges (fun x -> x / 2) 2 3 then "true" else "false");
	print_endline "testing (fun x -> x / 2) 2 2:";
	print_endline (if converges (fun x -> x / 2) 2 2 then "true" else "false")
