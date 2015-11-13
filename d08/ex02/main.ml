(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 21:55:55 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/12 22:26:51 by mcanal           ###   ########.fr       *)
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

  List.iter (fun x -> print_endline x#to_string) a_list
