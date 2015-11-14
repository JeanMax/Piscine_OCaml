(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   App.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/13 21:23:32 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 00:35:54 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type APP =
  sig
	type project = string * string * int
	val zero : project
	val combine : project -> project -> project
	val fail : project -> project
	val success : project -> project
  end


(* ************************************************************************** *)


module App : (APP with type project = string * string * int) =
  struct
	type project = string * string * int
	let zero = ("", "", 0)
	let combine x y =
	  let zboub = function
		  ((tx, _, gx), (ty, _, gy)) -> (tx^ty, (if (gx+gy)/2 > 80
												 then "succeed"
												 else "failed"), (gx+gy)/2)
	  in zboub (x, y)
	let fail = function (t, _, _) -> (t, "failed", 0)
	let success = function (t, _, _) -> (t, "succeed", 80)
  end


let print_proj (p:App.project) =
  match p with
	(t, s, g) -> print_endline ("type: " ^ t ^ "; status: " ^ s
								^ "; grade: " ^ (string_of_int g))



(* ************************************************************************** *)


let () =
  print_endline "testing zero:";
  print_proj (App.zero);

  print_endline "\ntesting fail (test, toto, 42):";
  print_proj (App.fail ("test", "toto", 42));

  print_endline "\ntesting success (test, toto, 42):";
  print_proj (App.success ("test", "toto", 42));

  print_endline "\ntesting combine (test, toto, 42) (test, toto, 42):";
  print_proj (App.combine ("test", "toto", 42) ("test", "toto", 42));

  print_endline "\ntesting combine (test, toto, 100) (test, toto, 62):";
  print_proj (App.combine ("test", "toto", 100) ("test", "toto", 62));
