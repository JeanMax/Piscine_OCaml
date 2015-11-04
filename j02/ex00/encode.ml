(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 22:05:09 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 19:27:20 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode l =
  let rec list_rev_append l1 l2 =
	match l1 with
	  [] -> l2
	| head :: tail -> list_rev_append tail (head :: l2)
  in let rec zboub li i ret =
	 match li with
	   []						-> ret
	 | h1::h2::tl when h1 = h2	-> zboub (h2::tl) (i + 1) ret
	 | h1::h2::tl				-> zboub (h2::tl) 1 ((i, h1) :: ret)
	 | hd::tl 					-> (i, hd) :: ret
   in zboub (list_rev_append l []) 1 []


let() =
  let rec print_list_int = function
	  []		-> ()
	| hd::tl	-> print_int hd; print_string " "; print_list_int tl

  in let rec print_list_tuple_int = function 
		 []				-> print_endline ""
	   | (i, j)::tl		-> print_int i; print_string ","; print_int j; 
						   print_string "; "; print_list_tuple_int tl

	 in let l0 = []
		and l1 = [1] 
		and l2 = [1; 2; 2; 3; 3; 3] 
		and l3 = [1; 2; 2; 3; 3; 3; 1; 1; 4; 4; 3; 2; 42] in 
		print_string "testing "; print_list_int l0; print_endline ":";
		print_list_tuple_int (encode l0);
		print_string "\ntesting "; print_list_int l1; print_endline ":";
		print_list_tuple_int (encode l1);
		print_string "\ntesting "; print_list_int l2; print_endline ":";
		print_list_tuple_int (encode l2);
		print_string "\ntesting "; print_list_int l3; print_endline ":";
		print_list_tuple_int (encode l3)
