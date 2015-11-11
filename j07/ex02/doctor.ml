(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:37:16 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 14:12:36 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor name age sidekick =
  object (self)

	val _name = name
	val _age = age
	val _sidekick : People.people = sidekick
	val _hp = 100

	method to_string = "name: " ^ _name ^ "... well, call me The Doctor"
					   ^ "; age: " ^ (string_of_int _age)
					   ^ "; hp: " ^ (string_of_int _hp)

	method talk = print_endline "Hi! I’m the Doctor!"

	method travel_in_time start arrival = 
	  print_endline 
"                        __---__
                       /__---__\\
                        |_|_|_|
                        |_|_|_|
         _______________|-----|_______________
        |                ¯¯¯¯¯                |
      __|_____________________________________|__
     |                                           |
   __|____________ _ ___ _ __________ _ _________|__
 ||  ||  |¯)(¯¯)|  | |¯ |_ ¯¯PUBLIC¯¯|_)(¯¯)\\/¯¯||  ||
 ||  ||__|¯_(__)|_ | |_ |_ ___CALL___|_)(__)/\\__||  ||
  |   ¯|¯¯¯¯¯¯¯¯¯¯ ¯ ¯¯¯¯¯|¯|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|¯   |
  |    | |¯¯¯¯|¯¯¯¯|¯¯¯¯| | | |¯¯¯¯|¯¯¯¯|¯¯¯¯| |    |
  |    | |    |    |    | | | |    |    |    | |    |
  |    | |    |    |    | | | |    |    |    | |    |
  |    | |____|____|____| | | |____|____|____| |    |
  |    | |    |    |    | | | |    |    |    | |    |
  |    | |    |    |    | | | |    |    |    | |    |
  |    | |    |    |    | | | |    |    |    | |    |
  |    | |____|____|____| | | |____|____|____| |    |
  |    | |______________| | | |              | |    |
  |    | | |¯¯¯¯¯¯¯¯¯¯| | | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |
  |    | | |   FREE   | | | | |              | |    |
  |    | | |          | | | |I|    _____     | |    |
  |    | | |  POLICE  |I| | |I|   ((¯¯¯))    | |    |
  |    | | |          |I| | |I|   ((___))    | |    |
  |    | | |          | | | | |    ¯¯¯¯¯     | |    |
  |    | | |          | | | | |              | |    |
  |    | | |__________| | | | |______________| |    |
  |    |  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯  | |O)                |    |
  |    | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |______________| | | |______________| |    |
  |    |                  | |                  |    |
  |    | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| | | |¯¯¯¯¯¯¯¯¯¯¯¯¯¯| |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |              | | | |              | |    |
  |    | |______________| | | |______________| |    |
  |    |                  | |                  |    |
|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|
|_____________________________________________________|";
	  new doctor _name (_age + arrival - start) _sidekick 
(*TODO: well, if You travel in time, Your age should be the same... nah?*)

	method use_sonic_screwdriver = 
	  print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

	method private regenerate = new doctor _name _age _sidekick

	method die = print_endline "We're all stories, in the end.";
				 self#regenerate 
	(* just to test private method *)

	initializer print_endline (_name ^ " is born!")
  end
