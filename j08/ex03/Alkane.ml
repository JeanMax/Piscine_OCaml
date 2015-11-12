(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 18:37:43 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 19:13:08 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual alkane n =
object

  val _name = match n with
	  1  -> "methane"
	| 2  -> "ethane"
	| 3  -> "propane"
	| 4  -> "butane"
	| 5  -> "pentane"
	| 6  -> "hexane"
	| 7  -> "heptane"
	| 8  -> "octane"
	| 9  -> "nonane"
	| 10 -> "decane"
	| 11 -> "undecane"
	| 12 -> "dodecane"
	| _  -> "smurfane"
	  
  val _formula = match n with
	  1  -> "CH4"
	| 2  -> "C2H6"
	| 3  -> "C3H8"
	| 4  -> "C4H10"
	| 5  -> "C5H12"
	| 6  -> "C6H14"
	| 7  -> "C7H16"
	| 8  -> "C8H18"
	| 9  -> "C9H20"
	| 10 -> "C10H22"
	| 11 -> "C11H24"
	| 12 -> "C12H26"
	| _  -> "smurfane"

  method name = _name
  method formula = _formula

  method to_string = "name: " ^ _name
					 ^ "; formula: " ^ _formula

  method equals (a:alkane) = a#formula = _formula
  
end
