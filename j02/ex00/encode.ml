(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 22:05:09 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 01:25:16 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode l =
 if l = [] then [] else
   let rec zboub li i ret =
	 match li with
	   h1::h2::tl when h1 = h2	-> zboub (List.tl li) (i + 1) ret
	 | h1::h2::tl				-> zboub (List.tl li) 1 ((i, h1) :: ret)
	 | _						-> (i, List.hd li) :: ret
   in zboub (List.rev l) 1 []


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
		print_string "testing "; print_list_int l1; print_endline ":";
		print_list_tuple_int (encode l1);
		print_string "testing "; print_list_int l2; print_endline ":";
		print_list_tuple_int (encode l2);
		print_string "testing "; print_list_int l3; print_endline ":";
		print_list_tuple_int (encode l3)
