(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/12 18:37:43 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/13 00:26:04 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let give_name = function
    1  -> "methane"                                                            
  | 2  -> "ethane"                                                             
  | 3  -> "propane"                                                            
  | 4  -> "butane"                                                             
  | 5  -> "pentane"                                                            
  | 6  -> "hexane"                                                             
  | 7  -> "heptane"                                                            
  | 8  -> "octane"                                                             
  | 9  -> "nonane"                                                             
  | 10 -> "decane"                                                             
  | 11 -> "undecane"                                                           
  | 12 -> "dodecane"                                                           
  | _  -> invalid_arg "Nah." 

let give_atom_list x =
  let make_atom_list atom n =
    let rec zboub i l =
      if i = 0 then l else zboub (i-1) (atom::l)
    in zboub n []
  in (make_atom_list new Atom.carbon x) 
	 @ (make_atom_list new Atom.hydrogen (2*x + 2))

class virtual alkane n =
object
  inherit Molecule.molecule (give_name n) (give_atom_list n)

  val _c = n
  val _h = 2*n + 2

  method get_c = _c
  method get_h = _h

end

class methane =
object
  inherit alkane 1
end

class ethane =
object
  inherit alkane 2
end

class propane =
object
  inherit alkane 3
end

class butane =
object
  inherit alkane 4
end

class pentane =
object
  inherit alkane 5
end

class hexane =
object
  inherit alkane 6
end

class heptane =
object
  inherit alkane 7
end

class octane =
object
  inherit alkane 8
end

class nonane =
object
  inherit alkane 9
end

class decane =
object
  inherit alkane 10
end

class undecane =
object
  inherit alkane 11
end

class dodecane =
object
  inherit alkane 12
end
