(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 23:18:30 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 23:44:26 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class ['a] army (crew:'a list) =
object

  val _crew = crew

  method add e = new army (e::_crew)
  method delete = new army (List.tl _crew)

end
