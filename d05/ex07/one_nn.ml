(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   one_nn.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 18:52:40 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 20:46:59 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* *********************************  ex05  ********************************* *)

let eu_dist a1 a2 =
  let ft_sum f start stop =
    let rec zboub n acc =
      if n > stop then acc else zboub (n + 1) (acc +. f n)
    in zboub start 0.0 in
  sqrt (ft_sum (fun i -> (a1.(i) -. a2.(i)) ** 2.) 0 (Array.length a1 - 1))

(* *********************************  ex06  ********************************* *)

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

(* *********************************  ex07  ********************************* *)

type radar = float array * string

let one_nn (rl:radar list) (r:radar) =
  let rec zboub nrst_class nrst_eu = function
	  []         -> nrst_class
	| head::tail -> let eu = eu_dist (fst r) (fst head)
					in if (eu < nrst_eu)
					   then zboub (snd head) eu tail
					   else zboub nrst_class nrst_eu tail
  in zboub "fail" 2147483647. rl

(* *********************************  test  ********************************* *)

let () =
  print_endline "testing with csv:";
  print_endline (one_nn 
				   (examples_of_file "../ex06/ionosphere.test.csv")
				   ([| 1.;0.;0.74916;0.02549;0.98994;0.09792;0.75855;0.12877;0.74313;-0.09188;0.95842;0.02482;0.97921;-0.00469;0.96110;0.10195;0.91482;0.03756;0.71026;0.02683;0.81221;-0.08048;1.;0.;0.71764;-0.01207;0.82271;0.02552;0.72435;-0.01073;0.90409;0.11066;0.72837;0.02750|], "g"));

  print_endline "\ntesting with hardcoded values:";
  print_endline (one_nn
				   ([ ([|1.; 1.; 1.; 1.; 1.|], "1");
					  ([|2.; 2.; 2.; 2.; 2.|], "2");
					  ([|3.; 3.; 3.; 3.; 3.|], "3");
					  ([|4.; 4.; 4.; 4.; 4.|], "4");
					  ([|5.; 5.; 5.; 5.; 5.|], "5") ])

				   ([|1.; 2.; 3.; 4.; 5.|], "0");)
