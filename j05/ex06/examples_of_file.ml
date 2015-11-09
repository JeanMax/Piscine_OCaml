(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 14:51:29 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 17:10:05 by mcanal           ###   ########.fr       *)
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
  in let a = (Array.make len "")
  in let rec fill_array i j =
	   let k = String.index_from s i ','
	   in Array.set a j (String.sub s i (k - i));
		  if j = len then a else fill_array k (j+1)
	 in fill_array 0 0

let examples_of_file path =
  let ic = open_in path
  in let rec read l =
	   try l :: (parse_line (input_line ic))
	   with Sys_error e -> print_endline ("fail: " ^ e)
		  | End_of_file -> close_in ic; l
	 in read []

let () = 
  let a = parse_line "1,0,0.94331,0.19959,0.96132,0.40803,0.80514,0.56569,0.56687,0.70830,0.41836,0.83230,0.14939,0.89489,0.05167,0.93682,-0.24742,0.83939,-0.42811,0.75554,-0.50251,0.62563,-0.65515,0.50428,-0.68851,0.30912,-0.77097,0.15619,-0.75406,-0.04399,-0.75199,-0.17921,-0.66932,-0.34367,g"
  in print_endline a.(0)
