(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 19:24:24 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 01:31:48 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class alkane_combustion ?(start=[])
						?(result=[])
						?(is_balanced=false)
						(a_list:Alkane.alkane list) =
object
  inherit Reaction.reaction start result

  val _is_balanced = is_balanced

  method get_start = 
	if _is_balanced then _start 
	else failwith "get_start: Reaction ain't balanced."

  method get_result = 
	if _is_balanced then _result 
	else failwith "get_result: Reaction ain't balanced."

  method is_balanced = _is_balanced

  method balance = 
	let rec list_remove_dup = function
		[]         -> []
	  | hd::tl -> hd::(list_remove_dup 
						 (List.filter (fun x -> x#name != hd#name) tl)) in
	let l = list_remove_dup a_list in
	let cl = List.map (fun x -> x#get_c) l in
	let hl = List.map (fun x -> x#get_h) l in
	let c = List.fold_left (+) (List.hd cl) (List.tl cl) in
	let h = List.fold_left (+) (List.hd hl) (List.tl hl) in
	let res = [((new Molecule.water), h/2); 
			   ((new Molecule.carbon_dioxyde), c)] in
	let sta = ((new Molecule.dioxygen), c + h/4)
			  ::(List.map (fun x -> ((x :> Molecule.molecule), 1)) l) in
	new alkane_combustion ~start:sta ~result:res 
		~is_balanced:(if h mod 4 != 0 then false else true) l

end
