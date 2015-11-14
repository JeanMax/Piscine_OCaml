(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Set.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/14 01:56:10 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 02:59:07 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type SET =
  sig
	type 'a t = 'a list
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b t) -> 'b t
	val union : 'a t -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val filter : 'a t -> ('a -> bool) -> 'a t
	val foreach : 'a t -> ('a -> unit) -> unit
	val for_all : 'a t -> ('a -> bool) -> bool
	val exists : 'a t -> ('a -> bool) -> bool
  end


(* ************************************************************************** *)


module Set : SET (*with type zboub*) =
  struct

	type 'a t = 'a list

	let return a = [a]

	let rec bind a f =
	  match a with
		[]         -> []
	  | head::tail -> (f head) @ bind tail f

	let inter a b = List.filter (fun x -> (List.mem x b)) a

	let diff a b = (List.filter (fun x -> not (List.mem x b)) a)
				   @ (List.filter (fun x -> not (List.mem x a)) b)

	let union a b = List.sort_uniq compare (a @ b)

	let filter a p = List.filter p a

	let foreach a f = List.iter f a

	let for_all a p = List.for_all p a

	let exists a p = List.exists p a

  end


(* ************************************************************************** *)


let () =
  print_endline "testing:";
