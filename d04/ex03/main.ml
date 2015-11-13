(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/07 19:40:51 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/07 21:55:04 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let(_,_) =

  let deck = Deck.newDeck () in
  print_endline "testing toStringList of newDeck:";
  List.iter (fun x -> print_string (x ^ "; ")) (Deck.toStringList deck);
  print_endline "\n\ntesting toStringListVerbose of newDeck:";
  List.iter (fun x -> print_string (x ^ "; ")) (Deck.toStringListVerbose deck);
  print_endline "\n\ntesting drawCard till it explodes:";
  let rec zboub = function
	  (c, d) -> try print_string ((Deck.Card.toString c)^"; ");
					zboub (Deck.drawCard d)
				with Failure e -> print_endline ("\nfail: "^e)		  
  in zboub (Deck.drawCard (Deck.newDeck ()));
	 
	 (Deck.Card.Value.all, Deck.Card.Color.all) (*propre!*)
