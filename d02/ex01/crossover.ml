(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 01:09:43 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 20:04:51 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover l1 l2 =
  let rec list_rev_append l1 l2 =
	match l1 with
	  []           -> l2
	| head::tail -> list_rev_append tail (head :: l2)
  in let list_filter p =
	let rec find acc = function
	  | []         -> list_rev_append acc []
	  | head::tail -> if p head then find (head :: acc) tail else find acc tail
	in find []
  in let rec list_mem x = function
		 []         -> false
	   | head::tail -> head = x || list_mem x tail

  in if l1 = [] || l2 = [] then []
  else list_filter (fun x -> list_mem x l2) l1

let() =
  let rec print_list_int = function
	  []		-> ()
	| hd::tl	-> print_int hd; print_string " "; print_list_int tl

  in let test l1 l2 =
	   print_string "testing "; print_list_int l1; print_string "and ";
	   print_list_int l2; print_endline ":";
	   print_list_int (crossover l1 l2); print_endline "";
	   
	 in let l0 = []
		and l1 = [1]
		and l2 = [1; 2; 3; 4; 5]
		and l3 = [2; 5; 21; 3; 42; 0; 21]
(*
		and l4 = [1; 2; 2; 3; 3; 3; 21; 3] 
		and l5 = [1; 2; 2; 3; 3; 3; 1; 1; 4; 4; 3; 2; 42]
*)
		in test l0 l0;
		   test l0 l1;
		   test l1 l2;
		   test l2 l3;
(*
		   test l4 l5;
*)
