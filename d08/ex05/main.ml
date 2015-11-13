(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 06:39:30 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_reac name = function
	[]     -> ()
  | hd::tl -> print_string (name ^ " + "
							^ (string_of_int (fst hd)) ^ "O2 -> " );
			  List.iter 
				(fun x -> print_string ( (if snd x != 1 
										  then (string_of_int (snd x))
										  else "")
										^ (fst x)#formula ^ " + " ))
						(snd hd);
			  print_endline "\b \b\b \b";
			  print_reac name tl

let () =
  print_endline "testing ethane:";
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.ethane] in
  print_reac "C2H6" (ac#balance)#get_incomplete_results;

  print_endline "\ntesting propane:";
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.propane] in
  print_reac "C3H8" (ac#balance)#get_incomplete_results;

  print_endline "\ntesting butane:";
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.butane] in
  print_reac "C4H10" (ac#balance)#get_incomplete_results;

  print_endline "\ntesting hexane:";
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.hexane] in
  print_reac "C6H14" (ac#balance)#get_incomplete_results;


  print_endline "\ntesting hexane + butane:";
  let ac = new Alkane_combustion.alkane_combustion [new Alkane.hexane;
													new Alkane.butane] in
  print_reac "C6H14 + C4H10" (ac#balance)#get_incomplete_results;

