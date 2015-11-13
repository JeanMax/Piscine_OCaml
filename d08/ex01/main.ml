(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 22:28:52 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let m_list = [new Molecule.trinitrotoluene;
				new Molecule.water;
				new Molecule.carbon_dioxyde;
				new Molecule.tetrahydrocannabinol;
				new Molecule.citric_acid;
				new Molecule.carbonic_acid] in
  
  List.iter (fun x -> print_endline x#to_string) m_list
