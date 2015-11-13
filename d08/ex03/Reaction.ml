(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 19:14:19 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 19:47:11 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction (start:(Molecule.molecule * int) list) 
					   (result:(Molecule.molecule * int) list) =
object

  val _start = start
  val _result = result

  method virtual get_start : (Molecule.molecule * int) list
  method virtual get_result : (Molecule.molecule * int) list
  method virtual balance : reaction
  method virtual is_balanced : bool

end
