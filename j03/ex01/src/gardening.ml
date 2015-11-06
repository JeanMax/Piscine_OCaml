(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 03:48:28 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 05:50:54 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec size = function
	Leaf                 -> 0
  | Node(left, _, right) -> 1 + (size left) + (size right)

let rec height = function  
	Leaf                 -> 0
  | Node(left, _, right) -> 1 + max (height left) (height right)


let draw_tree =
	let rec zboub x y = function
		Leaf                 -> ()
	  | Node(left, s, right) -> Graphics.draw_circle x y 24;
								Graphics.moveto (x - 12) (y - 8);
								Graphics.draw_string s;
								if left != Leaf then begin
									Graphics.moveto (x + 76) (y - x/4 + 80);
									Graphics.lineto (x + 24) y end
								else Graphics.moveto (x + 24) y;
								if right != Leaf then
								  Graphics.lineto (x + 76) (y + x/4 - 80);
								zboub (x + 100) 
									  (y - x/4 + 80)
									  left;
								zboub (x + 100)
									  (y + x/4 - 80)
									  right
		in zboub 120 225


let() =
  let tree = Node(
				 Node(
					 Node(Leaf, "l-l", Leaf),
					 "l",
					 Node(Leaf, "l-r", Leaf)),
				 "root",
				 Node(
					 Node(Leaf, "r-l", Leaf),
					 "r",
					 Node(Leaf,
						  "r-r",
						  Node(Leaf, "r-r-r", Leaf))))

  in Graphics.open_graph "";
	 Graphics.moveto 10 10;
     Graphics.draw_string 
	   ("size:"^string_of_int(size tree)^" height:"^string_of_int(height tree));
	 draw_tree tree;
	 let rec debug_coord (x, y) =
	   ignore (read_line ());
	   Printf.printf "x:%d y:%d" x y; 
	   debug_coord (Graphics.mouse_pos ())
	 in debug_coord (Graphics.mouse_pos ())
					 
