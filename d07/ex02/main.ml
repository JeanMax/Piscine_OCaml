(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:29:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 01:41:06 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let d = new Dalek.dalek () in
  print_endline "\ntesting to_string:";
  print_endline d#to_string;
  print_endline "\ntesting talk:";
  d#talk;
  print_endline "\ntesting exterminate:";
  d#exterminate (new People.people "Rose");
  print_endline "\ntesting die:";
  ignore (d#die)
  
