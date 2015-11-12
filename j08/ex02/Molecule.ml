(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 18:27:33 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 18:32:27 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual molecule name formula =
object

  val _name = name
  val _formula = formula

  method name = _name
  method formula = _formula

  method to_string = "name: " ^ _name
					 ^ "; formula: " ^ _formula

  method equals (m:molecule) = m#formula = _formula
											 
end
