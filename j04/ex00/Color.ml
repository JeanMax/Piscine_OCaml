(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 19:02:04 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 20:24:53 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString = function
	Spade    -> "S"
   | Heart   -> "H"
   | Diamond -> "D"
   | Club    -> "C"

let toStringVerbose = function
	Spade    -> "Spade"
   | Heart   -> "Heart"
   | Diamond -> "Diamond"
   | Club    -> "Club"
