(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 13:07:31 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/10 16:04:03 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let count_coma str =
  let r = ref 0 in
  for i = 0 to (String.length str) - 1 do
	if str.[i] = ',' then incr r
  done;
  !r

let joke path =
  Random.self_init ();
  let ic = open_in path in
  let s = input_line ic in
  close_in ic;
  let len = count_coma s + 1 in
  let a = (Array.make len "") in
  let start = ref 0 in
  if len = 1
  then (Array.set a 0 s; print_endline a.(0))
  else 
	let stop = ref (String.index s ',') in
	  for i = 0 to len - 1
	  do
		Array.set a i (String.sub s !start (!stop - !start));
		start := !stop + 1;
		if i >= (len - 2)
		then stop := (String.length s)
		else stop := (String.index_from s !start ',');
	  done;
	  print_endline (a.(Random.int len))
					
let () =
  if Array.length Sys.argv = 2 then joke (Array.get Sys.argv 1)
