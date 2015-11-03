(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/03 01:15:12 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/03 01:24:29 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n =
  match n with
	z when z < 0 -> (-1)
  | 0 -> 1
  | _ -> n - hfs_m (hfs_f (n -1))
and hfs_m n =
  match n with
	z when z < 0 -> (-1)
  | 0 -> 0
  | _ -> n - hfs_f (hfs_m (n -1))

let() =
  print_endline "testing m -42: "; print_int (hfs_m (-42));
  print_endline "\ntesting m 0: "; print_int (hfs_m 0);
  print_endline "\ntesting m 1: "; print_int (hfs_m 1);
  print_endline "\ntesting m 2: "; print_int (hfs_m 2);
  print_endline "\ntesting m 3: "; print_int (hfs_m 3);
  print_endline "\ntesting m 4: "; print_int (hfs_m 4);
  print_endline "\n\ntesting f -42: "; print_int (hfs_f (-42));
  print_endline "\ntesting f 0: "; print_int (hfs_f 0);
  print_endline "\ntesting f 1: "; print_int (hfs_f 1);
  print_endline "\ntesting f 2: "; print_int (hfs_f 2);
  print_endline "\ntesting f 3: "; print_int (hfs_f 3);
  print_endline "\ntesting f 4: "; print_int (hfs_f 4);
  print_endline ""
