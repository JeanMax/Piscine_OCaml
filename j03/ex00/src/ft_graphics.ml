(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/05 16:11:58 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 04:51:02 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a tree * 'a * 'a tree
									 
let draw_square x y size =
  Graphics.moveto (x - (size/2)) (y - (size/2));
  Graphics.lineto (x - (size/2)) (y + (size/2));
  Graphics.lineto (x + (size/2)) (y + (size/2));
  Graphics.lineto (x + (size/2)) (y - (size/2));
  Graphics.lineto (x - (size/2)) (y - (size/2))
				  
let draw_tree_node = function
  	Nil           -> draw_square 200 225 100;
					 Graphics.moveto 185 225;
					 Graphics.draw_string "Nil"

  | Node(_, v, _) -> draw_square 400 150 100;
					 draw_square 400 300 100;
					 draw_square 200 225 100;
					 Graphics.moveto 350 300;
					 Graphics.lineto 250 225;
					 Graphics.lineto 350 150;
					 Graphics.moveto 185 225;
					 Graphics.draw_string v;
					 Graphics.moveto 400 300;
					 Graphics.draw_string "Nil";
					 Graphics.moveto 400 150;
					 Graphics.draw_string "Nil"
	

let() =
  Graphics.open_graph "";
  let s = read_line ()
  in if s = "Nil" || s = "nil" || s = "NIL" || s = ""
	 then draw_tree_node Nil
	 else draw_tree_node (Node (Nil, s, Nil));
	 ignore(read_line ());

