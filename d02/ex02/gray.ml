(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 01:51:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 19:20:20 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let gray n =
  begin 
	if n > 0 then
	  let rec list_iter f = function
			 []         -> ()
		   | head::tail -> f head; list_iter f tail
	  in let rec list_map f = function
			 []         -> []
		   | head::tail -> (f head) :: list_map f tail
	  in let rec list_rev_append l1 l2 =
		match l1 with
		  []         -> l2
		| head::tail -> list_rev_append tail (head :: l2)
	  in let list_append l1 l2 =
		   list_rev_append (list_rev_append l1 []) l2

	  in let rec zboub i =
		   if i = 1 then ["0"; "1"] else
			 let l = zboub (i - 1)
			 in list_append (list_map (fun x->"0"^x) l)
							(list_rev_append (list_map (fun x->"1"^x) l) [])
		 in list_iter (fun x->print_string x; print_char ' ') (zboub n)
  end; print_char '\n'

let() =
  print_endline "testing -1:"; gray (-1);
  print_endline "testing 0:"; gray 0;
  print_endline "testing 1:"; gray 1;
  print_endline "testing 2:"; gray 2;
  print_endline "testing 3:"; gray 3;
  print_endline "testing 4:"; gray 4;
  print_endline "testing 8:"; gray 8
