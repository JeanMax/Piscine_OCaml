(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 19:24:24 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 06:51:01 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class alkane_combustion ?(start=[])
						?(result=[])
						?(is_balanced=false)
						(a_list:Alkane.alkane list) =
object (self)
  inherit Reaction.reaction start result

  val _is_balanced = is_balanced
  val _a_list =
    let rec list_remove_dup = function
        []         -> []
	  | hd::tl     -> hd::(list_remove_dup
							 (List.filter (fun x -> x#name != hd#name) tl))
	in list_remove_dup a_list
	

  method get_start = 
	if _is_balanced then _start 
	else failwith "get_start: Reaction ain't balanced."

  method get_result = 
	if _is_balanced then _result 
	else failwith "get_result: Reaction ain't balanced."

  method is_balanced = _is_balanced

  method balance = 
	let cl = List.map (fun x -> x#get_c) _a_list in
	let hl = List.map (fun x -> x#get_h) _a_list in
	let c = List.fold_left (+) (List.hd cl) (List.tl cl) in
	let h = List.fold_left (+) (List.hd hl) (List.tl hl) in
	let res = [((new Molecule.water), h/2); 
			   ((new Molecule.carbon_dioxyde), c)] in
	let sta = ((new Molecule.dioxygen), c + h/4)
			  ::(List.map (fun x -> ((x :> Molecule.molecule), 1)) _a_list) in
	new alkane_combustion ~start:sta ~result:res 
		~is_balanced:(if h mod 4 != 0 then false else true) _a_list


  method get_incomplete_results =
	let cl = List.map (fun x -> x#get_c) _a_list in
	let n = List.fold_left (+) (List.hd cl) (List.tl cl) in

	let make_mol_list = function
		(0,    0,   0)  -> []
	  | (0,    0,   nC) -> [(new Molecule.soot, nC);
							(new Molecule.water, (n+1))]
	  | (0,    nCO, 0)  -> [(new Molecule.carbon_monoxyde, nCO);
							(new Molecule.water, (n+1))]
	  | (nCO2, 0,   0)  -> [(new Molecule.carbon_dioxyde, nCO2);
							(new Molecule.water, (n+1))]
	  | (nCO2, nCO, 0)  -> [(new Molecule.carbon_dioxyde, nCO2);
							(new Molecule.carbon_monoxyde, nCO);
							(new Molecule.water, (n+1))]
	  | (nCO2, 0,   nC) -> [(new Molecule.carbon_dioxyde, nCO2);
							(new Molecule.soot, nC);
							(new Molecule.water, (n+1))]
	  | (0,    nCO, nC) -> [(new Molecule.carbon_monoxyde, nCO);
							(new Molecule.soot, nC);
							(new Molecule.water, (n+1))]
	  | (nCO2, nCO, nC) -> [(new Molecule.carbon_dioxyde, nCO2);
							(new Molecule.carbon_monoxyde, nCO);
                            (new Molecule.soot, nC);
							(new Molecule.water, (n+1))] in

	let rec dec_O2 nO nO2 l1 =
      let rec dec_CO2 nCO2 l2 =
		if nCO2 >= 0 then
          begin
			if n + nCO2 - nO >= 0
			then dec_CO2 (nCO2-1) ((nO2, 
								   (make_mol_list (nCO2,
												   (nO - 2*nCO2), 
												   (n + nCO2 - nO))))::l2)
			else  dec_CO2 (nCO2-1) l2
          end 
		else l2
	  in
	  if nO > 0
	  then dec_O2 (nO-2) (nO2-1) ((dec_CO2 (nO/2) []) @ l1)
	  else l1 in
	dec_O2 (n+1) (n+1) []

end
