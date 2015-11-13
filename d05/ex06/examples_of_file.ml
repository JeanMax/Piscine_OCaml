(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 14:51:29 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 19:13:18 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let count_coma s =
  let rec count i n =
	if i < 0 then n 
	else if s.[i] = ',' then count (i-1) (n+1)
	else count (i-1) n
  in count (String.length s - 1) 0

let parse_line s =
  let len = count_coma s
  in let a = (Array.make len 0.)
  in let rec fill_array i start stop =
	   Array.set a i (float_of_string (String.sub s start (stop - start)));
	   if i = (len-1) 
	   then (a, (String.sub s (stop+1) (String.length s - stop - 1)))
	   else fill_array (i+1) (stop+1) (String.index_from s (stop+1) ',')
	 in fill_array 0 0 (String.index s ',')

let examples_of_file path =
  let ic = open_in path
  in let rec read l =
	   try read ((parse_line (input_line ic)) :: l)
	   with Sys_error e -> print_endline ("fail: " ^ e); []
		  | End_of_file -> close_in ic; List.rev l
	 in read []

let () = 
  let rec test = function
	  []         -> ()
	| head::tail ->  match head with
					   (x, y) -> 
					   for i = 0 to Array.length x -1 do
						 print_float x.(i);
						 print_string ","
					   done;
					   print_endline y;
					   test tail
  in test (examples_of_file "ionosphere.test.csv")
