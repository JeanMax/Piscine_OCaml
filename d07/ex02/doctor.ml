(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:37:16 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 23:51:03 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor ?(hp=100) name age sidekick =
object (self)
		 
  val _name = name
  val _age = age
  val _sidekick : People.people = sidekick
  val _hp = hp
			  
  method to_string = "name: " ^ _name ^ "... well, call me The Doctor"
					 ^ "; age: " ^ (string_of_int _age)
					 ^ "; hp: " ^ (string_of_int _hp)
									
  method talk = print_endline "Hi! I’m the Doctor!"

  method travel_in_time start arrival = 
	print_endline 
("                        __---__\n                       /__---__\\\n                        |_|_|_|\n                        |_|_|_|\n         _______________|-----|_______________\n        |                ¯¯¯¯¯                |\n      __|_____________________________________|__\n     |                                           |\n   __|____________ _ ___ _ __________ _ _________|__\n ||  ||  |¯)(¯¯)|  | |¯ |_ ¯¯PUBLIC¯¯|_)(¯¯)\\/¯¯||  ||\n ||  ||__|¯_(__)|_ | |_ |_ ___CALL___|_)(__)/\\__||  ||\n  |   ¯|¯¯¯¯¯¯¯¯¯¯ ¯ ¯¯¯¯¯|¯|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|¯   |\n  |    | |¯¯¯¯|¯¯¯¯|¯¯¯¯| | | |¯¯¯¯|¯¯¯¯|¯¯¯¯| |    |\n  |    | |    |    |    | | | |    |    |    | |    |\n  |    | |    |    |    | | | |    |    |    | |    |\n  |    | |____|____|____| | | |____|____|____| |    |\n  |    | |    |    |    | | | |    |    |    | |    |\n  |    | |    |    |    | | | |    |    |    | |    |\n  |    | |    |    |    | | | |    |    |    | |    |\n  |    | |____|____|____| | | |____|____|____| |    |\n  |    | |______________| | | |              | |    |\n  |    | | |¯¯¯¯¯¯¯¯¯¯| | | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |\n  |    | | |   FREE   | | | | |              | |    |\n  |    | | |          | | | |I|    _____     | |    |\n  |    | | |  POLICE  |I| | |I|   ((¯¯¯))    | |    |\n  |    | | |          |I| | |I|   ((___))    | |    |\n  |    | | |          | | | | |    ¯¯¯¯¯     | |    |\n  |    | | |          | | | | |              | |    |\n  |    | | |__________| | | | |______________| |    |\n  |    |  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯  | |O)                |    |\n  |    | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |______________| | | |______________| |    |\n  |    |                  | |                  |    |\n  |    | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |              | | | |              | |    |\n  |    | |______________| | | |______________| |    |\n  |    |                  | |                  |    |\n|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|\n|_____________________________________________________|\nJust traveled in time from " ^ (string_of_int start)
 ^ " to " ^ (string_of_int arrival));
	new doctor _name _age _sidekick 
  (*well, if You travel in time, Your age should be the same... nah?*)

  method use_sonic_screwdriver = 
	print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

  method private regenerate = new doctor _name _age _sidekick

  (* just to test private method *)
  method hurt x = print_endline "Ouch!";
				  new doctor ~hp:(_hp - x) _name _age _sidekick

  method die = print_endline "We're all stories, in the end.";
			   self#regenerate 

  initializer print_endline (_name ^ " is born!")
end
