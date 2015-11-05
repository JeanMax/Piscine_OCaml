(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 10:25:21 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/05 21:52:24 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let list_rev l =
  let rec list_rev_append l1 l2 =
    match l1 with
	  []         -> l2
    | head::tail -> list_rev_append tail (head :: l2)
  in list_rev_append l []

(* ********************************   ex04   ******************************** *)

type phosphate   = string
type deoxyribose = string
type nucleobase  = A | T | C | G | None | U
type nucleotide  = { p:phosphate; d:deoxyribose; b:nucleobase }

let generate_nucleotide n =
  {
	p = "phosphate";
	d = "deoxyribose";
	b = match n with 
		  'A' -> A
		| 'T' -> T
		| 'C' -> C
		| 'G' -> G
		| _	  -> None
  }
  
(* ********************************   ex05   ******************************** *)

type helix = nucleotide list

let generate_helix n =
  Random.self_init (); (* TODO: move this somewhere else? (test!?) *)
  let rec zboub i l =
    if i <= 0 then l
    else zboub (i - 1) (begin generate_nucleotide 
								begin match (Random.int 4) with
                                        0 -> 'A'
                                      | 1 -> 'T'
                                      | 2 -> 'C'
                                      | 3 -> 'G'
                                      | _ -> '0'
								end
						end :: l)
  in zboub n []
		   
let helix_to_string hel =
  let rec zboub str h =
	   match h with 
		 []       -> str
	   | hd :: tl -> zboub (str ^ "p=" ^ hd.p ^ ", d=" ^ hd.d ^ ", b=" ^ 
							  begin match hd.b with
									  A    -> "A"
									| T    -> "T"
									| C    -> "C"
									| G    -> "G"
									| _ -> "None"
							  end ^ ";\n") tl
	 in zboub "" hel

let complementary_helix hel =
  let rec zboub h com =
	match h with
      []       -> list_rev com
	| hd :: tl -> zboub tl (generate_nucleotide
							  begin match hd.b with
									    A    -> 'T'
                                      | T    -> 'A'
                                      | C    -> 'G'
                                      | G    -> 'C'
                                      | _ -> '0'
							  end :: com)
	in zboub hel []


(* ********************************   ex06   ******************************** *)

type rna = nucleobase list

let generate_rna hel =
  let rec zboub h rna =
	match h with
	  []       -> list_rev rna
	| hd :: tl -> zboub tl (begin match hd.b with
									A    -> U
								  | T    -> A
								  | C    -> G
								  | G    -> C
								  | _    -> None 
							end :: rna)
  in zboub hel []

(* ********************************   test   ******************************** *)

let() =
  let string_of_rna rna =
	let rec zboub r str =
	  match r with
		[]       -> str
	  | hd :: tl -> zboub tl (str ^ begin match hd with
											A    -> "A"
										  | T    -> "T"
										  | C    -> "C"
										  | G    -> "G"
										  | U    -> "U"
										  | _ -> "None"
									end ^ "; ")	  
	in zboub rna ""
  in for i = -1 to 3 do 
	   let h = generate_helix i in
	   print_endline ("\ntesting "^(string_of_int i)^":");
	   print_endline ("-helix:\n"^(helix_to_string h)); 
	   print_endline ("-rna:\n"^(string_of_rna (generate_rna h)))
	 done
