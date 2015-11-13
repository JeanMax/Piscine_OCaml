(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/10 18:21:46 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/11 01:35:58 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = 
  sig 
	val pair : (int * int)
  end

module type VAL = 
  sig 
	val x : int 
  end

(* FIX ME !!! *)

module type MAKEPROJECTION =
  functor (Pair:PAIR) -> VAL
	
module MakeFst : MAKEPROJECTION =
  functor (Pair:PAIR) ->
  struct
	let x = fst Pair.pair
  end

module MakeSnd : MAKEPROJECTION =
  functor (Pair:PAIR) -> 
  struct 
	let x = snd Pair.pair
  end

(* FIX ME !!! *)

module Pair : PAIR = 
  struct 
	let pair = ( 21, 42 ) 
  end

module Fst : VAL = 
  MakeFst (Pair)

module Snd : VAL = 
  MakeSnd (Pair)


let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
