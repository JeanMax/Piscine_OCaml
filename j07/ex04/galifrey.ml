(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   galifrey.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/11 23:53:30 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 01:09:53 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class galifrey (dalek_l:Dalek.dalek list) 
			   (doctor_l:Doctor.doctor list)  
			   (people_l:People.people list)  =
object (self)

  val _dalek_l = dalek_l
  val _doctor_l = doctor_l
  val _people_l = people_l

  method do_time_war = 
	print_endline "The war starts!\n";
	let rec zboub = function
		([], _, []) -> "Arg! They're all dead... tie?"
	  | (_, _, [])  -> "The daleks won the war!"
	  | ([], _, _)   -> "The doctor (and his minions) won the war!"
	  | (da::da_t, dr, pe::pe_t) -> let doc = List.hd dr in
									doc#talk; 
									doc#use_sonic_screwdriver; doc#die;
									da#talk; da#exterminate pe;
									pe#talk; da#die;
									print_endline "";
		 zboub (da_t, dr, pe_t)		 
	  | _            -> "Dang! War is broken..."

	in print_endline (zboub (_dalek_l, _doctor_l, _people_l))

end
