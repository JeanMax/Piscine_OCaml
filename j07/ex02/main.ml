(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:29:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 14:11:31 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let d = new Doctor.doctor "$!*$@" 909 (new People.people "Amy Pond") in
  print_endline "\ntesting to_string:";
  print_endline d#to_string;
  print_endline "\ntesting talk:";
  d#talk;
  print_endline "\ntesting travel in time 2015 3141:";
  print_endline ((d#travel_in_time 2015 3141)#to_string);
  print_endline "\ntesting use_sonic_screwdriver:";
  d#use_sonic_screwdriver;
  print_endline "\ntesting regenerate (die):";
  print_endline ((d#die)#to_string)


