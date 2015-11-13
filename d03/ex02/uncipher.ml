(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 14:31:22 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 16:26:45 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let uncaesar n s =
  Cipher.caesar (-n) s

let unrot42 s =
  Cipher.caesar (-42) s

let xor n s = (*TODO: remove?*)
  Cipher.xor n s

let ft_uncrypt s fl =
	let rec list_rev_append l1 l2 =
	  match l1 with
		[]         -> l2
	  | head::tail -> list_rev_append tail (head :: l2)
	in Cipher.ft_crypt s (list_rev_append fl [])




let() =
  let s = "Ushhh... this is a secret string! :O" in
  print_endline ("String to crypt: '" ^ s ^ "'");
  print_endline ("rot42:   '" ^ 
				   (Cipher.rot42 s) ^ "'");
  print_endline ("unrot42: '" ^ 
				   (unrot42 (Cipher.rot42 s)) ^ "'");
  print_endline ("caesar 12:   '" ^ 
				   (Cipher.caesar 12 s) ^ "'");
  print_endline ("uncaesar 12: '" ^ 
				   (uncaesar 12 (Cipher.caesar 12 s)) ^ "'");
  print_endline ("caesar 4242:   '" ^ 
				   (Cipher.caesar 4242 s) ^ "'");
  print_endline ("uncaesar 4242: '" ^ 
				   (uncaesar 4242 (Cipher.caesar 4242 s)) ^ "'");
  print_endline ("caesar -4242:   '" ^ (Cipher.caesar (-4242) s) ^ "'");
  print_endline ("uncaesar -4242: '" ^ 
				   (uncaesar (-4242) (Cipher.caesar (-4242) s)) ^ "'");
  print_endline ("xor 42:     '" ^ 
				   (Cipher.xor 42 s) ^ "'");
  print_endline ("'un'xor 42: '" ^ 
				   (xor 42 (Cipher.xor 42 s)) ^ "'");
  print_endline ("ft_crypt [xor 42; caesar 42]:     '" ^ 
				   (Cipher.ft_crypt s [(Cipher.xor 42); (Cipher.caesar 42)]) ^ "'");
  print_endline ("ft_uncrypt [xor 42; uncaesar 42]: '" ^ 
				   (ft_uncrypt 
					  (Cipher.ft_crypt s [(Cipher.xor 42); (Cipher.caesar 42)])
					  [(xor 42); (uncaesar 42)]) ^ "'")
