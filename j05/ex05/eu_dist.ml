(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/09 13:46:25 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/09 14:15:30 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let eu_dist a1 a2 =
  let ft_sum f start stop =
    let rec zboub n acc =
      if n > stop then acc else zboub (n + 1) (acc +. f n)
    in zboub start 0.0 in
  sqrt (ft_sum (fun i -> (a1.(i) -. a2.(i)) ** 2.) 0 (Array.length a1 - 1))

let () =
  let a1 = [| 1.; 2.; 3. |] in
  let a2 = [| 1.; 2.; 1. |] in
  let a3 = [| 1.; 2.; 3. |] in
  let a4 = [| 3.; 2.; 1. |] in
  print_endline ("testing eu_dist 1;2;3 1;2;1:\n"
				 ^ (string_of_float (eu_dist a1 a2)));
  print_endline ("\ntesting eu_dist 1;2;3 3;2;1:\n"
				 ^ (string_of_float (eu_dist a3 a4)))
