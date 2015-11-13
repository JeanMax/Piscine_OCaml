(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 19:14:19 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 22:51:38 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction (s_list:(Molecule.molecule * int) list) 
					   (r_list:(Molecule.molecule * int) list) =
object

  method virtual get_start : (Molecule.molecule * int) list
  method virtual get_result : (Molecule.molecule * int) list
  method virtual balance : reaction
  method virtual is_balanced : bool
  
end
