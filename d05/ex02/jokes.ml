(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 13:07:31 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/10 16:12:40 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Random.self_init ();
  let a = [|"C'est un schtroumpf,\nil court,\nil tombe,\nil se fait un bleu.";
			"C'est l'histoire d'une blague vaseuse\nMets tes bottes.";
			"C'est l'histoire d'un poil, avant il etait bien, maintenant il est pubien.";
			"C'est l'histoire d'un zoophile, il rentre dans un bar.";
			"C'est quoi le plus dur a mixer dans un légume ?\nLe fauteuil."|] in
  print_endline (a.(Random.int 5))
