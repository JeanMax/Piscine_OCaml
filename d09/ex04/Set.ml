(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Set.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/14 01:56:10 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 18:05:11 by mcanal           ###   ########.fr       *)
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

let print_list l =
  List.iter (fun x -> print_int x; print_string "; ") l;
  print_endline ""

let () =
  let l1 = [1;2;3;4;5] in
  let l2 = [4;5;6;7;8] in
  print_string "l1: ";
  print_list l1;
  print_string "l2: ";
  print_list l2;

  print_endline "\ntesting return 42:";
  print_list (Set.return 42);
  
  print_endline "\ntesting bind l1 (x2):";
  print_list (Set.bind l1 (fun x -> [2 * x]));

  print_endline "\ntesting union l1 l2:";
  print_list (Set.union l1 l2);
  
  print_endline "\ntesting inter l1 l2:";
  print_list (Set.inter l1 l2);
  
  print_endline "\ntesting diff l1 l2:";
  print_list (Set.diff l1 l2);

  print_endline "\ntesting filter >3 l1:";
  print_list (Set.filter l1 (fun x -> x > 3));
  
  print_endline "\ntesting foreach print_int l1:";
  Set.foreach l1 print_int;
  
  print_endline "\n\ntesting for_all >3 l1:";
  print_endline (string_of_bool (Set.for_all l1 (fun x -> x > 3)));

  print_endline "\ntesting for_all >3 l2:";
  print_endline (string_of_bool (Set.for_all l2 (fun x -> x > 3)));

  print_endline "\ntesting exist 3 l1:";
  print_endline (string_of_bool (Set.exists l1 (fun x -> x = 3)));

  print_endline "\ntesting exist 3 l2:";
  print_endline (string_of_bool (Set.exists l2 (fun x -> x = 3)));
