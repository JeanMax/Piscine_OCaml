
let list_hd = function
	[]         -> failwith "hd"
  | head::tail -> head

let list_tl = function
	[]         -> failwith "tl"
  | head::tail -> tail

let rec list_mem x = function
	[]         -> false
  | head::tail -> head = x || mem x tail

let rec list_iter f = function
	[]         -> ()
  | head::tail -> f head; list_iter f tail

let rec list_rev_append l1 l2 =
  match l1 with
	[]         -> l2
  | head::tail -> list_rev_append tail (head :: l2)

let list_rev l = list_rev_append l []

let rec list_map f = function
	[]         -> []
  | head::tail -> (f head) :: list_map f tail
		  
let list_filter p = 
  let rec find acc = function
	  []         -> list_rev acc
	| head::tail -> if p head then find (head :: acc) tail else find acc tail
  in find [] 

let list_rev_map f l =
  let rec rmap_f acc = function
	  []         -> acc
	| head::tail -> rmap_f (f head :: acc) tail
  in rmap_f [] l

let list_append l1 l2 =
  list_rev_append (list_rev_append l1 []) l2

let string_concat c =
  let rec con s = function
	  []         -> s
	| head::tail -> con (if s = "" then head else s ^ c ^ head) tail
  in con ""