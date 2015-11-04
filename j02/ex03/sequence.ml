(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 02:48:42 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 03:23:57 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sequence n = 
  if n < 0 then "" else
	let rec zboub li i ret =
      match li with
		h1::h2::tl when h1 = h2  -> zboub (List.tl li) (i + 1) ret
      | h1::h2::tl               -> zboub (List.tl li) 1 (i :: h1 :: ret)
      | _                        -> i :: (List.hd li) :: ret
	in let rec zgeg m l =
		 if m = 0 then l else zgeg (m - 1) (zboub (List.rev l) 1 [])
	   in String.concat "" (List.map string_of_int (zgeg n [1]))

let() = 
  print_endline "testing -1:"; print_endline (sequence (-1));
  print_endline "testing 0:"; print_endline (sequence 0);
  print_endline "testing 1:"; print_endline (sequence 1);
  print_endline "testing 2:"; print_endline (sequence 2);
  print_endline "testing 3:"; print_endline (sequence 3);
  print_endline "testing 4:"; print_endline (sequence 4);
  print_endline "testing 5:"; print_endline (sequence 5);
  print_endline "testing 6:"; print_endline (sequence 6)

  
