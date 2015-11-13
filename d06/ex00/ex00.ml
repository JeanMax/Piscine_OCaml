(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex00.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/10 16:25:22 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/10 16:39:51 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringSet = Set.Make(String)
						  
let () =
  let set = List.fold_right
			  StringSet.add
			  [ "foo"; "bar"; "baz"; "qux" ]
			  StringSet.empty
  in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
