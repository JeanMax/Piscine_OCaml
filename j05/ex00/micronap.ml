(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 11:48:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 12:25:50 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ocaml unix.cma micronap.ml *)

let my_sleep () = Unix.sleep 1

let () =
  let av = Sys.argv in
  if Array.length av = 2
  then let i = try (int_of_string av.(1))
			   with Failure e -> 0 in
	   for j = i downto 1
	   do
		 my_sleep ();
	   done
