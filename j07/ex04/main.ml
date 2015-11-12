(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 13:29:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 01:31:27 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Random.self_init ();
  let ga = new Galifrey.galifrey 
			   [new Dalek.dalek ();
				new Dalek.dalek ();
				new Dalek.dalek ()] 
			   [new Doctor.doctor "$!*$@" 909 (new People.people "Amy")] 
			   [new People.people "Vastra";
				new People.people "Rose";
				new People.people "Clara";
				new People.people "Strax"] in
  ga#do_time_war
