(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 12:26:45 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 13:51:31 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = { mutable contents : 'a }

let return x =
 { contents = x }

let get r =
  r.contents

let set r x = 
  r.contents <- x
  
let bind r f:('b ft_ref) =
  f r.contents

let () =
  let r = return 42 in
  print_endline ("testing return 42:\n" ^ (string_of_int r.contents));
  set r 41; 
  print_endline ("\ntesting set 41 / get:\n" ^ (string_of_int (get r)));
  print_endline ("\ntesting bind r +1:\n"
				 ^ (string_of_int (get (bind r (fun x -> return (x+1))))))
