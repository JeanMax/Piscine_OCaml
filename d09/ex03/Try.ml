(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/14 00:33:19 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 17:19:29 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module type TRY =
  sig
	type 'a t = Success of 'a | Failure of exn
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
	val flatten : 'a t t -> 'a t
  end


(* ************************************************************************** *)


module Try : TRY (*with type zboub*) =
  struct

    type 'a t = Success of 'a | Failure of exn

    let return a = Success a

    let bind t f =
	  match t with
		Success a -> (try f a with e -> Failure e)
	  | Failure e -> Failure e

    let recover t f =
	  match t with
		Failure e -> f e
	  | _         -> t

    let filter t f =
	  match t with
		Success a when f a = false -> Failure (failwith "Try.filter exn")
	  | _                          -> t

    let flatten = function
		Success a -> begin 
						match a with
						  Success _ -> a
						| _         -> Failure (failwith "Try.fatten exn")
					 end
	  | _ -> Failure (failwith "Try.fatten exn")
	  
  end
	
	
(* ************************************************************************** *)

	
let print_try = function
	Try.Success a -> print_endline ("Success "^a)
  | Try.Failure _  -> print_endline ("Failure")

let useless_fun a =
  if a = "crash" then (failwith "badaboum")
  else Try.Success (a^a)

let () =
  let t = Try.return "42" in
  let crash = Try.return "crash" in

  print_endline "testing return 42:";
  print_try t;

  print_endline "\ntesting bind 42:";
  print_try (Try.bind t useless_fun);

  print_endline "\ntesting bind crash:";
  let f = (Try.bind crash useless_fun) in
  print_try f;

  print_endline "\ntesting recover 42:";
  print_try (Try.recover t (fun x -> Success "failure"));

  print_endline "\ntesting recover Failure:";
  print_try (Try.recover f (fun x -> Success "failure"));

  print_endline "\ntesting filter 42:";
  print_try (Try.filter f (fun x -> x = "42"));

  print_endline "\ntesting filter crash:";
  print_try (Try.filter t (fun x -> x = "42"));

  print_endline "\ntesting flatten S/S:";
  print_try (Try.flatten (Try.Success t));

  print_endline "\ntesting flatten S/F:";
  try print_try (Try.flatten (Try.Success f))
  with Failure e -> print_endline e;
	   
