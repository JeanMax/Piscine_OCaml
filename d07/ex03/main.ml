(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:29:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 01:45:37 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  let da = new Dalek.dalek () in
  let pe = new People.people "Rose" in
  let dr = new Doctor.doctor "$!*$@" 909 pe in

  let dr_army = new Army.army [dr] in
  let da_army = new Army.army [da] in
  let pe_army = new Army.army [pe] in

  let dr_army = dr_army#add dr in
  let da_army = da_army#add da in
  let pe_army = pe_army#add pe in

  if dr_army#get_crew != [] && da_army#get_crew != [] && pe_army#get_crew != []
  then print_endline "Army created";

  let dr_army = dr_army#delete in
  let dr_army = dr_army#delete in
  let da_army = da_army#delete in
  let da_army = da_army#delete in
  let pe_army = pe_army#delete in
  let pe_army = pe_army#delete in

  if dr_army#get_crew = [] && da_army#get_crew = [] && pe_army#get_crew = []
  then print_endline "Army destroyed"
