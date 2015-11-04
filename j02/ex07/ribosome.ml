(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ribosome.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/04 10:55:06 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/04 12:29:24 by mcanal           ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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
      []       -> List.rev com
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
	  []       -> List.rev rna
	| hd :: tl -> zboub tl (begin match hd.b with
									A    -> U
								  | T    -> A
								  | C    -> G
								  | G    -> C
								  | _    -> None 
							end :: rna)
  in zboub hel []

(* ********************************   ex07   ******************************** *)

type aminoacid = Ala | Arg | Asn | Asp | Cys | Gln | Glu | 
				 Gly | His | Ile | Leu | Lys | Met | Phe |
				 Pro | Ser | Thr | Trp | Tyr | Val | Stop
type protein   = aminoacid list

let generate_bases_triplets rna =
  let rec zboub l tri =
	match l with
	  h::n1::n2::t -> zboub t ((h, n1, n2)::tri)
	| _            -> List.rev tri
  in zboub rna []

let decode_arn rna =
  let rec zboub tri pro =
	match tri with
	  ((U, A, A) | (U, A, G) | (U, G, A))::t                                     -> List.rev (Stop::pro)
    | ((G, C, A) | (G, C, C) | (G, C, G) | (G, C, U))::t                         -> zboub t (Ala::pro)
    | ((A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U))::t -> zboub t (Arg::pro)
    | ((A, A, C) | (A, A, U))::t                                                 -> zboub t (Asn::pro)
    | ((G, A, C) | (G, A, U))::t                                                 -> zboub t (Asp::pro)
    | ((U, G, C) | (U, G, U))::t                                                 -> zboub t (Cys::pro)
    | ((C, A, A) | (C, A, G))::t                                                 -> zboub t (Gln::pro)
    | ((G, A, A) | (G, A, G))::t                                                 -> zboub t (Glu::pro)
    | ((G, G, A) | (G, G, C) | (G, G, G) | (G, G, U))::t                         -> zboub t (Gly::pro)
    | ((C, A, C) | (C, A, U))::t                                                 -> zboub t (His::pro)
    | ((A, U, A) | (A, U, C) | (A, U, U))::t                                     -> zboub t (Ile::pro)
    | ((C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G))::t -> zboub t (Leu::pro)
    | ((A, A, A) | (A, A, G))::t                                                 -> zboub t (Lys::pro)
    | ((A, U, G))::t                                                             -> zboub t (Met::pro)
    | ((U, U, C) | (U, U, U))::t                                                 -> zboub t (Phe::pro)
    | ((C, C, C) | (C, C, A) | (C, C, G) | (C, C, U))::t                         -> zboub t (Pro::pro)
    | ((U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C))::t -> zboub t (Ser::pro)
    | ((A, C, A) | (A, C, C) | (A, C, G) | (A, C, U))::t                         -> zboub t (Thr::pro)
    | ((U, G, G))::t                                                             -> zboub t (Trp::pro)
    | ((U, A, C) | (U, A, U))::t                                                 -> zboub t (Tyr::pro)
    | ((G, U, A) | (G, U, C) | (G, U, G) | (G, U, U))::t                         -> zboub t (Val::pro)
	| _ -> print_endline "fail"; [] (* all cases are alreday handled :D *)
  in zboub (generate_bases_triplets rna) []

let string_of_protein pro =
    let rec zboub l str =
      match l with
        []       -> str
      | hd :: tl -> zboub tl (str ^ begin match hd with
                                            Ala -> "Alanine"
										  | Arg -> "Arginine"
										  | Asn -> "Asparagine"
										  | Asp -> "Aspartique"
										  | Cys -> "Cysteine"
										  | Gln -> "Glutamine"
										  | Glu -> "Glutamique"
										  | Gly -> "Glycine"
										  | His -> "Histidine"
										  | Ile -> "Isoleucine"
										  | Leu -> "Leucine"
										  | Lys -> "Lysine"
										  | Met -> "Methionine"
										  | Phe -> "Phenylalanine"
										  | Pro -> "Proline"
										  | Ser -> "Serine"
										  | Thr -> "Threonine"
										  | Trp -> "Tryptophane"
										  | Tyr -> "Tyrosine"
										  | Val -> "Valine"
										  | Stop -> "End of translation"
                                    end ^ "; ")
    in zboub pro ""

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
  in let h = generate_helix 1000 in
	 print_endline ("\ntesting "^(string_of_int 1000)^":");
	 print_endline ("-helix:\n"^(helix_to_string h)); 
	 print_endline ("-rna:\n"^(string_of_rna (generate_rna h)));
	 print_endline ("-protein:\n"^(string_of_protein (decode_arn (generate_rna h))))
	   
