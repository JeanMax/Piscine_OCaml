(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:20:15 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 13:35:09 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people name =
  object (self)

	val _name = name
	val _hp = 100

	method to_string = "name: " ^ _name
					   ^ "; hp: " ^ (string_of_int _hp)

	method talk = print_endline ("I'm " ^ _name
								 ^ "! Do you know the Doctor?")

	method die = print_endline "Aaaarghh!"

	initializer print_endline (_name ^ " is born!")
  end
