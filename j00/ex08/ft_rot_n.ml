(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/01 20:26:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 17:39:05 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n n str =
	let rot c =
	  match c with
		 'a'..'z' -> char_of_int ((int_of_char c + n - 97) mod 26 + 97)
	   | 'A'..'Z' -> char_of_int ((int_of_char c + n - 65) mod 26 + 65)
	   | _		  -> c
	in String.map rot str

let() =
  print_endline "testing 1 abcdefghijklmnopqrstuvwxyz:";
  print_endline(ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline "\ntesting 1 ABCDEFGHIJKLMNOPQRSTUVWXYZ:";
  print_endline(ft_rot_n 1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  print_endline "\ntesting 13 :abcdefghijklmnopqrstuvwxyz";
  print_endline(ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline "\ntesting 42 0123456789:";
  print_endline(ft_rot_n 42 "0123456789");
  print_endline "\ntesting 2 OI2EAS67B9:";
  print_endline(ft_rot_n 2 "OI2EAS67B9");
  print_endline "\ntesting 0 Damned !:";
  print_endline(ft_rot_n 0 "Damned !");
  print_endline "\ntesting 42 :";
  print_endline(ft_rot_n 42 "");
  print_endline "\ntesting 1 NBzlk qnbjr !:";
  print_endline(ft_rot_n 1 "NBzlk qnbjr !")
