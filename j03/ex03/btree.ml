(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 16:48:08 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/07 16:20:29 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec tree_less x = function
	Nil                   -> true
  | Node (v, left, right) -> v < x && (tree_less x left) && (tree_less x right)
	
let rec tree_gtr x = function
	Nil                   -> true
  | Node (v, left, right) -> v > x && (tree_gtr x left) && (tree_gtr x right)

let rec tree_max = function
	Node (x, _, Nil)     -> x
  | Node (_, _, right)   -> tree_max right
  | _                    -> failwith "tree_max: called on Nil"

let rec height = function  
	Nil                   -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

let rec is_bst = function
	Nil                   -> true
  | Node (v, left, right) -> is_bst left && is_bst right
							 && (tree_less v left) && (tree_gtr v right)
 
let rec is_perfect = function
	Nil                   -> true
  | Node (_, left, right) -> height left = height right
							 && is_perfect left && is_perfect right

let rec is_balanced = function
	Nil                   -> true
  | Node (_, left, right) -> abs (height left - height right) <= 1
							 && is_balanced left && is_balanced right

let rec search_bst x = function
	Nil                   -> false
  | Node (v, left, right) -> if x < v then search_bst x left
							 else if x = v then true
							 else search_bst x right

let rec add_bst x = function
	Nil                     -> Node (x, Nil, Nil)
  | Node (v, left, right)   -> 
	 if x = v then failwith "add_bst: value already exists"
	 else if x < v then Node (v, add_bst x left, right)
	 else Node (v, left, add_bst x right)

let rec delete_bst x = function
	Nil                   -> failwith "delete_bst: value not found"
  | Node (v, left, right) -> 
	 if v = x then
	   begin 
		 match (left, right) with
		   (Nil, Nil)     -> Nil
		 | (left , Nil)   -> left
		 | (Nil, right)   -> right
		 | _              -> let m = tree_max left
							 in Node (m, delete_bst m left, right)
	   end
	 else if x < v then Node (v, delete_bst x left, right)
	 else Node (v, left, delete_bst x right)




let test_tree t =
  print_endline ("testing is_bst:        " ^ (string_of_bool (is_bst t)));
  print_endline ("testing is_perfect:    " ^ (string_of_bool (is_perfect t)));
  print_endline ("testing is_balanced:   " ^ (string_of_bool (is_balanced t)));
  print_endline ("testing search_bst 42: " ^ (string_of_bool (search_bst 42 t))
  ^ "\n")


let () =
  print_endline "test-tree: Nil->0->42->-42";
  test_tree (add_bst (-42) (add_bst 42 (add_bst 0 Nil)));

  print_endline "test-tree: Nil->0->42->-42  del 42";
  test_tree (delete_bst 42 (add_bst (-42) (add_bst 42 (add_bst 0 Nil))));

  print_endline "test-tree: Nil->0->42->-42->3";
  test_tree (add_bst 3 (add_bst (-42) (add_bst 42 (add_bst 0 Nil))));

  print_endline "test-tree: Nil->0->41->-42->3";
  test_tree (add_bst 3 (add_bst (-42) (add_bst 41 (add_bst 0 Nil))));

  print_endline "test-tree: Nil->0->41->-42->3->7";
  test_tree (add_bst 7 (add_bst 3 (add_bst (-42) (add_bst 41 (add_bst 0 Nil)))));

  print_endline "test-tree: Nil->0->42->-42->42 ";
  try test_tree (add_bst 42 (add_bst (-42) (add_bst 42 (add_bst 0 Nil))));
  with Failure e -> print_endline ("Failure caught: "^e^"\n");

  print_endline "test-tree: Nil->0->42->-42  del 420";
  try test_tree (delete_bst 420 (add_bst (-42) (add_bst 42 (add_bst 0 Nil))));
  with Failure e -> print_endline ("Failure caught: "^e^"\n");
