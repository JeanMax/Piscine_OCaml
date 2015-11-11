(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 20:14:29 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 20:54:41 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek ?(hp=100) ?(shield=true) name =
	object (self)

	val _name = name
	val _shield = shield
	val _hp = hp
	val _sentences = ["Explain! Explain!";
					  "Exterminate! Exterminate!";
					  "I obey!";
					  "You are the Doctor! You are the enemy of the Daleks!"]

	method talk = print_endline (_sentences.(Random.int 3));
	  
	method to_string = "name: " ^ _name ^ "... well, call me The Doctor"
					   ^ "; shield: " ^ (string_of_bool _shield)
					   ^ "; hp: " ^ (string_of_int _hp)

	method exterminate =
	  
	method die = print_endline "Emergency Temporal Shift!"
				
	end

	  
