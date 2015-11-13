(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:20:15 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 00:49:41 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people ?(hp=100) name =
object
		 
  val _name = name
  val _hp = hp

  method get_hp = _hp
			  
  method to_string = "name: " ^ _name
					 ^ "; hp: " ^ (string_of_int _hp)
									  
  method talk = print_endline ("I'm " ^ _name
							   ^ "! Do you know the Doctor?")

  method die = print_endline "Aaaarghh!";
			   new people ~hp:(0) _name

(*  initializer print_endline (_name ^ " is born!") *)
end
