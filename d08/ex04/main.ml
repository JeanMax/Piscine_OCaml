(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 01:51:03 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_reac r =
  List.iter 
	(fun x -> print_string ((string_of_int (snd x)) ^ (fst x)#formula ^ " "))
	r#get_start;
  print_string "-> ";
  List.iter 
	(fun x -> print_string ((string_of_int (snd x)) ^ (fst x)#formula ^ " "))
	r#get_result;
  print_endline ""

let () =
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.methane] in
  print_reac ac#balance;

  let ac = new Alkane_combustion.alkane_combustion [new Alkane.propane] in
  print_reac ac#balance;

  let ac = new Alkane_combustion.alkane_combustion [new Alkane.methane;
													new Alkane.methane;
													new Alkane.propane] in
  print_reac ac#balance;

  let ac = new Alkane_combustion.alkane_combustion [new Alkane.methane;
													new Alkane.methane;
													new Alkane.undecane;
													new Alkane.propane] in
  print_reac ac#balance;

  let ac = new Alkane_combustion.alkane_combustion [new Alkane.ethane] in
  try print_reac ac#balance with Failure e -> print_endline e;

