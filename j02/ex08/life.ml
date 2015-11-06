(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   life.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: mcanal <zboub@42.fr>                       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/11/05 21:34:02 by mcanal            #+#    #+#             *)
(*   Updated: 2015/11/06 19:23:05 by mcanal           ###   ########.fr       *)
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
  Random.self_init ();
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

(* ********************************   ex07   ******************************** *)

type aminoacid = Ala | Arg | Asn | Asp | Cys | Gln | Glu | 
				 Gly | His | Ile | Leu | Lys | Met | Phe |
				 Pro | Ser | Thr | Trp | Tyr | Val | Stop
type protein   = aminoacid list

let generate_bases_triplets rna =
  let rec zboub l tri =
	match l with
	  h::n1::n2::t -> zboub t ((h, n1, n2)::tri)
	| _            -> list_rev tri
  in zboub rna []

let decode_arn rna =
  let rec zboub tri pro =
	match tri with
	  ((U, A, A) | (U, A, G) | (U, G, A))::t                                     -> list_rev (Stop::pro)
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

(* ********************************   ex08   ******************************** *)

let string_of_helix l =
  if l = [] then ""
  else
	let rec loop li s =
	  match li with
        h::t -> loop t (begin
						   match h.b with
							 A -> "A"
						   | T -> "T"
						   | C -> "C"
						   | G -> "G"
						   | U -> "U"
						   | None -> "-"
						 end
						^ s)
      | _ -> s
    in loop l ""

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
								  end )
  in zboub rna ""

let helix_of_string s =
  if s ="" then [] else
	let rec loop str n helix =
	  if n < 0 then helix
	  else loop str (n - 1) ((generate_nucleotide str.[n])::helix)
	in loop s ((String.length s) - 1) []
			
let life s =
  let h = helix_of_string s in
  print_endline "We just generated that pretty helix for you:";
  print_endline (string_of_helix h);
  print_endline "\nHere the complementary helix:";
  print_endline (string_of_helix (complementary_helix h));
  print_endline "\nAnd the RNA working with that helix:";
  print_endline (string_of_rna (generate_rna h));
  print_endline "\nFinally, we got a protein:";
  print_endline (string_of_protein (decode_arn (generate_rna h)))
  
(* ********************************   test   ******************************** *)

let() =
  life "ATACACAGTGGATAAGCGTACAACCCGAAAGGGTAAGCAGAACTGGGCTTGTTGCATTCGTGATTCCTAGGGACGTGTTACGCTATTACCAAGCAACAAATATAAATTTATAAACCGTCTCTGCCGATACCATTGGCTTTGCTGTTCGGCGATGCGGAAATCGCCATATGATCGGCCTCCATCTAACTGGTTTTCCCTACGACAAGGGATCGCACATTGGGCACCCCAGATTGCCCTAATACTTCGGGCACTAAGACATTAAGTCGAAACACTTTGCATCCGTGTCGTATCTAACTAATCTTATACAATCTCATTCCCTCTCGGAAATTAGTCTCGGTAAATTCACCGACAGAAGTGACCGAGGAAGCAGTACTAAATAGGGACGCTTTTATTGTAACGTCATTCGGGCCTAAGATTACTTTTATCCTCTATAGTGAATAGGTCCGCAATTTTGTCTCTCTAACGAGGCGCACACCGGTGGTCGCCGTAAGGGCGGTTGATGTACTAGGCGCTTCTAATGTTCTGCAGCAACCGACTGGGGGAAACTAATCAGTTTAGATAACAGAGGATGCCCGCACCATAGCATGAAGTTCCCGAGCTGGCTACAAAATGCACCCTGCGGCCTGCAGAGTAGTTTAATCGTGAAAGAACGGGGTCTCCTCACTATATTGGTTCGGCGAATTTCCACATGTTCAGCCGAGGGACCCTTGGACACATTTAGCACCGTTAGGCATGAGATCCTAATGCTAGTTAGGGAGGGAATACTGTTAGTCGACTACCATAGAGGCTAACAGAAATTTAGGATGGCACAGAAGAGTAATACAAGGCACTATGGGACCTATATCCCGCCCAACGCTTTCTCTTGGATGATACCAGTATATTCAATTGTATTCCTATGGAAAATCTAGCGTCACAGCGACAGCTAATGACAACTCACAAAGGAACCTCTTTTGTGCAATCCCTATTGTTGCGGACTTCAGGGCCAGAGCGGGTCCAAGTC"
