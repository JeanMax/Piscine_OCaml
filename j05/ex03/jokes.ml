(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 13:07:31 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 16:54:46 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Random.self_init ();
  let a = [| ""; ""; ""; ""; "" |] in
  let ic = open_in "jokes.txt" in
  for i = 0 to 4 
  do
	try Array.set a i (input_line ic);
	with Sys_error e -> print_endline ("fail: " ^ e)
	   | End_of_file -> print_endline "file too short"
  done;
  print_endline (a.(Random.int 5));
  close_in ic
