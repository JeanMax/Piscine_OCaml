(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 19:01:44 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let m_list = [new Molecule.trinitrotoluene;
				new Molecule.water;
				new Molecule.carbon_dioxyde;
				new Molecule.tetrahydrocannabinol;
				new Molecule.citric_acid;
				new Molecule.carbonic_acid] in
  
  List.iter (fun x -> print_endline x#to_string) m_list;

  print_endline "\ntesting THC = THC:";
  print_endline (string_of_bool
				   ((new Molecule.tetrahydrocannabinol)#equals
									   (new Molecule.tetrahydrocannabinol)));

  print_endline "\ntesting THC = H2O:";
  print_endline (string_of_bool
				   ((new Molecule.tetrahydrocannabinol)#equals
													   (new Molecule.water)))
