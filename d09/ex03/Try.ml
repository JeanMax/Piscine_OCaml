(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/14 00:33:19 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/14 02:07:08 by mcanal           ###   ########.fr       *)
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
	
	
let () =
  print_endline "testing :"
				
