(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/06 03:48:28 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 20:45:50 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size = function
	Nil                  -> 0
  | Node(_, left, right) -> 1 + (size left) + (size right)

let rec height = function  
	Nil                  -> 0
  | Node(_, left, right) -> 1 + max (height left) (height right)


let draw_tree =
	let rec zboub x y = function
		Nil                  -> ()
	  | Node(s, left, right) -> Graphics.draw_circle x y 50;
								Graphics.moveto (x - 12) (y - 8);
								Graphics.draw_string s;
								if left != Nil then begin
									Graphics.moveto (x + 100) (y - x/3 + 145);
									Graphics.lineto (x + 50) y end
								else Graphics.moveto (x + 50) y;
								if right != Nil then
								  Graphics.lineto (x + 100) (y + x/3 - 145);
								zboub (x + 150) 
									  (y - x/3 + 145)
									  left;
								zboub (x + 150)
									  (y + x/3 - 145)
									  right
		in zboub 120 225


let() =
  let tree = Node(
				 "root",
				 Node(
					 "l",
					 Node("l-l", Nil, Nil),
					 Nil),
				 Node(
					 "r",
					 Node("r-l", Nil, Nil),
					 Node("r-r",
						  Nil,
						  Nil)))

  in Graphics.open_graph "";
	 Graphics.moveto 10 10;
     Graphics.draw_string 
	   ("size:"^string_of_int(size tree)^" height:"^string_of_int(height tree));
	 draw_tree tree;
	 ignore (read_line ())
