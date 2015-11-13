(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 19:45:36 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let a_list = [new Alkane.methane;
				new Alkane.ethane;
				new Alkane.propane;
				new Alkane.butane;
				new Alkane.pentane;
				new Alkane.hexane;
				new Alkane.heptane;
				new Alkane.octane;
				new Alkane.nonane;
				new Alkane.decane;
				new Alkane.undecane;
				new Alkane.dodecane] in 

  List.iter (fun x -> print_endline x#to_string) a_list;

  let m = ((new Alkane.methane) :> Molecule.molecule) in
  let e = ((new Alkane.ethane) :> Molecule.molecule) in
  
  print_endline "\ntesting ethane = ethane:";
  print_endline (string_of_bool (e#equals e));

  print_endline "\ntesting methane = ethane:";
  print_endline (string_of_bool (m#equals e))
