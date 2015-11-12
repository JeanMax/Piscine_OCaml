(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 19:24:24 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 19:47:30 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class alkane_combustion a_list =
object

  method get_start : (Molecule.molecule * int) list
  method get_result : (Molecule.molecule * int) list
  method balance : reaction
  method is_balanced : bool
  
end
