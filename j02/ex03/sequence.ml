(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 02:48:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 20:27:24 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sequence n = 
  if n < 0 then "" else
	let rec list_map f = function
		[]         -> []
	  | head::tail -> (f head) :: list_map f tail
	in let rec list_rev_append l1 l2 =
		 match l1 with
		   []         -> l2
		 | head::tail -> list_rev_append tail (head :: l2)
	in let string_concat c =
		 let rec con s = function
			 []         -> s
		   | head::tail -> con (if s = "" then head else s ^ c ^ head) tail
		 in con ""
	in let rec zboub li i ret =
		 match li with
		   []                        -> ret
		 | h1::h2::tl when h1 = h2  -> zboub (h2::tl) (i + 1) ret
		 | h1::h2::tl               -> zboub (h2::tl) 1 (i :: h1 :: ret)
		 | hd::tl                   -> i :: (hd) :: ret
	   in let rec zgeg m l =
			if m = 0 then l else zgeg (m - 1) (zboub (list_rev_append l []) 1 [])
		  in string_concat "" (list_map string_of_int (zgeg n [1]))

let() = 
  print_endline "testing -1:"; print_endline (sequence (-1));
  print_endline "testing 0:"; print_endline (sequence 0);
  print_endline "testing 1:"; print_endline (sequence 1);
  print_endline "testing 2:"; print_endline (sequence 2);
  print_endline "testing 3:"; print_endline (sequence 3);
  print_endline "testing 4:"; print_endline (sequence 4);
  print_endline "testing 5:"; print_endline (sequence 5);
  print_endline "testing 6:"; print_endline (sequence 6)

  
