(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 22:12:59 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 22:15:24 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let a_list = [new Atom.hydrogen;
				new Atom.helium;
				new Atom.lithium;
				new Atom.beryllium;
				new Atom.boror;
				new Atom.carbon;
				new Atom.nitrogen;
				new Atom.oxygen;
				new Atom.fluorine;
				new Atom.neon] in
  List.iter (fun x -> print_endline x#to_string) a_list
