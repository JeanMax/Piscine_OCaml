(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 20:14:29 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 01:29:12 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek ?(hp=100) () =
object
		 
  val _hp = hp
  val _name = "Dalek"
			  ^ String.make 1 (char_of_int ((Random.int 26) + 65))
			  ^ String.make 1 (char_of_int ((Random.int 26) + 97))
			  ^ String.make 1 (char_of_int ((Random.int 26) + 97))
  val mutable _shield = true
					 
  method talk = 
	let zboub = function
		0  -> "Explain! Explain!"
	  | 1  -> "Exterminate! Exterminate!"
	  | 2  -> "I obey!"
	  | _  -> "You are the Doctor! You are the enemy of the Daleks!"
	in print_endline (zboub (Random.int 3))
		
  method to_string = 
	"name: " ^ _name
	^ "; shield: " ^ (string_of_bool _shield)
	^ "; hp: " ^ (string_of_int _hp)
									  
  method exterminate (p:People.people) = 
	_shield <- not _shield;
	p#die
	
  method die = 
	print_endline "Emergency Temporal Shift!";
	new dalek ~hp:(0) ()
							 
end

	  
