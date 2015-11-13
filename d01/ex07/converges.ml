(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 01:37:47 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 19:11:54 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n =
  if (n < 0) then false 
  else if (x = f x) then true 
  else converges f (n - 1) (f x)

let() =
	print_endline "testing (( * ) 2) 2 5:";
	print_endline (if converges (( * ) 2) 2 5 then "true" else "false");
	print_endline "testing (fun x -> x / 2) 2 3:";
	print_endline (if converges (fun x -> x / 2) 2 3 then "true" else "false");
	print_endline "testing (fun x -> x / 2) 2 2:";
	print_endline (if converges (fun x -> x / 2) 2 2 then "true" else "false")
