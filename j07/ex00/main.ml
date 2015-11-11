(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:29:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 13:34:51 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let p = new People.people "toto" in
  print_endline "\ntesting to_string:";
  print_endline p#to_string;
  print_endline "\ntesting talk:";
  p#talk;
  print_endline "\ntesting die:";
  p#die
