(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/10 16:40:26 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/10 17:26:00 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHash =
  struct
	type t = string
			   
	let equal (s1:t) (s2:t) =
	  s1 = s2

	(* sdbm algo *)
	let hash (s:t) =
	  let len = String.length s in
	  let rec zboub i h =
		if i = len then h
		else zboub (i+1) ((int_of_char s.[i]) + (h lsl 6) + (h lsl 16) + h)
	  in zboub 0 0
  end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
