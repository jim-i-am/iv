open Core.Std

exception ParseError
exception EvalError

(** ADT for binary operators *)
type bin_op = ADD | SUB | MUL | DIV | POW

(** ADT for valid expressions *)
type expr = 
	  Scalar of float
	| Vector of float list
	| BinOp of expr * bin_op * expr

(** ADT for valid tokens *)
type expr_elem =
	  Texp of expr
	| Tbin of bin_op
	| Tscan of bin_op
	| Treduce of bin_op
	| Tlparen

(** ADT for valid statements*)
type statement =
	| Assign of string * expr
	| Eval of expr

type value = 
	| ScalarVal of float
	| VectorVal of float list
	| Unit

(** extracts a token from the given string *)
let token str i j = String.sub str i (j - i)

(** extract a float value *)
let extract_float cmd i j =
	let v = token cmd i j |> Float.of_string in (v, j)

(** parses the float value  *)
let parse_float cmd i n = 
	let rec aux j = 
		if j = n then extract_float cmd i j
		else match cmd.[j] with
			| '0'..'9' | '.' -> aux (j+1)
			| _ ->	extract_float cmd i j
	in aux i

(**  parse the variable name *)
let parse_var cmd i n = 
	let rec aux j = 
		if j = n then (token cmd i j, j) 
		else match cmd.[j] with
			| 'a'..'z' | 'A'..'Z' -> aux (j + 1)
			| _ -> (token cmd i j, j)
	in aux i

(** the priorities for binary operator precedence *)
let pr_op = function
	| ADD -> 1
	| SUB -> 1
	| MUL -> 2
	| DIV -> 2
	| POW -> 3

(** match the corresponding chat to a binary operator *)
let match_op = function
	| '+' -> ADD
	| '-' -> SUB
	| '*' -> MUL
	| '/' -> DIV
	| '^' -> POW
	| _ -> raise ParseError

(** adds a float token to the lexeme stack w.r.t to the stack's head.  *)
let add_float e = function
	| (Texp (Scalar s)) :: tl -> (Texp (Vector [e;s])) :: tl
	| (Texp (Vector v)) :: tl -> (Texp (Vector (e :: v))) :: tl
	| _ as l -> (Texp (Scalar e)) :: l
	
(** adds a lex token to the lexeme stack w.r.t operator precedence *)
let add_bin_op new_op lex = 
	let e = Tbin new_op in
	match lex with	
		| (Texp r) :: (Tbin op) :: (Texp l) :: tl 
			when (pr_op op) >= pr_op new_op -> 
				let bin_expr = BinOp(r, op, l) in e :: (Texp bin_expr) :: tl 
		| ((Texp l) :: tl) as st -> e :: st
		| _ -> raise ParseError

(** the lexer that creates the lexeme stack *)
let lexify cmd = 
	let n = String.length cmd in
	let rec aux lex i =
		if i=n then lex
		else match cmd.[i] with
			| '0'..'9' -> let v, j = parse_float cmd i n in 
							let l = add_float v lex in aux l j
			| '(' -> let s = Tlparen :: lex and j = i + 1 in aux s j
			| ')' -> (match lex with
						| (Texp r) :: (Tbin op) :: (Texp l) :: Tlparen :: tl -> 
							let s = Texp (BinOp (l,op,r))::tl in aux s (i+1)												
						| (Texp r) :: (Tbin op) :: (Texp l) :: tl -> 
							let s = Texp (BinOp (l,op,r)) :: tl in aux s i
						| (Texp e) :: (Tlparen) :: t -> 
							let s = (Texp e) :: t in aux s (i+1)
						| _ -> raise ParseError)
			| '+' | '-' | '*' | '/' | '^' as c -> 
						let op = match_op c in 
							let l = add_bin_op op lex and j = i + 1
								in aux l j
			| ' ' -> aux lex (i+1)	
			| _ -> raise ParseError	
	in aux [] 0

(** reduces lexems into a singular abstract syntax tree,
	where the value returned is the root *)
let reduce lex = 
	let rec aux = function 
		| (Texp l) :: (Tbin op) :: (Texp r) :: tl -> 
			let tree = Texp(BinOp(l, op, r)) :: tl in aux tree
		| [(Texp l)] -> l 
		| _ -> raise ParseError
	in aux lex

(** parses a statement into lexemes using pushdown automata and then
	reduces to a single abstract syntax tree *)
let parse cmd = lexify cmd |> reduce

(** performs addition evaluation *)
let add l r = 
	match (l, r) with
		| (ScalarVal x, ScalarVal y) -> ScalarVal (x +. y)
		| (ScalarVal s, VectorVal v) | (VectorVal v, ScalarVal s) -> 
			VectorVal (List.map v (fun x -> x +. s))
		| (VectorVal x, VectorVal y) -> VectorVal (List.map2_exn x y (+.))
		| _ -> raise EvalError

(** performs subtraction evaluation *)
let sub l r = 
	match (l, r) with
		| (ScalarVal x, ScalarVal y) -> ScalarVal (y -. x)
		| (ScalarVal s, VectorVal v) -> VectorVal ( List.map v (fun x -> x -. s) )
		| (VectorVal v, ScalarVal s) -> VectorVal ( List.map v (fun x -> s -. x) )
		| (VectorVal x, VectorVal y) -> VectorVal ( List.map2_exn x y (-.) )
		| _ -> raise EvalError		

(** performs multiplication evaluation *)
let mul l r = 
	match (l, r) with
		| (ScalarVal x, ScalarVal y) -> ScalarVal (x *. y)
		| (ScalarVal s, VectorVal v) | (VectorVal v, ScalarVal s) -> 
				VectorVal ( List.map v (fun x -> x *. s) )
		| (VectorVal x, VectorVal y) -> VectorVal (List.map2_exn x y ( *. ))
		| _ -> raise EvalError		

(** performs division evaluation *)
let div l r = 
	match (l, r) with
		| (ScalarVal x, ScalarVal y) -> ScalarVal (y /. x)
		| (ScalarVal s, VectorVal v) -> VectorVal (List.map v (fun x -> x /. s))
		| (VectorVal v, ScalarVal s) -> VectorVal (List.map v (fun x -> s /. x))
		| (VectorVal x, VectorVal y) -> VectorVal (List.map2_exn x y (/.))
		| _ -> raise EvalError		

(** performs exponent evaluation *)
let pow l r = 
	match (l, r) with
		| (ScalarVal x, ScalarVal y) -> ScalarVal (y ** x)
		| (ScalarVal s, VectorVal v) -> VectorVal (List.map v (fun x -> x ** s))
		| (VectorVal v, ScalarVal s) -> VectorVal (List.map v (fun x -> s ** x))
		| (VectorVal x, VectorVal y) -> VectorVal (List.map2_exn x y ( ** ))
		| _ -> raise EvalError		

(** evaluates binary operators *)
let eval_op x y = function
	| ADD -> add x y
	| SUB -> sub x y
	| MUL -> mul x y
	| DIV -> div x y 
	| POW -> pow x y

(** evaluates a abstact syntax tree *)
let rec eval = function
	| Scalar v -> ScalarVal v
	| Vector l -> VectorVal (List.rev l)
	| BinOp (l,op,r) -> let x = eval l and y = eval r in eval_op x y op

(** an inefficient to_string impl for vector values. *)
let to_string l =
	let rec aux str = function
		| [] -> str
		| h :: tl -> aux (str ^ " " ^ (Float.to_string h)) tl 
	in aux "" l

(** pretty print the value to output. *)
let pp = function
	| ScalarVal i -> Float.to_string i
	| VectorVal l -> to_string l
	| Unit -> "()"

(** the chain for executing commands. *)
let exec cmd = parse cmd |> eval |> pp 

(** Top level reads input, parses, evaluates and prints the result. *)
let main =
	print_endline "welcome to the iv calculator" ;
	try
		while true do
			print_string "iv> ";
			read_line () |> exec |> print_endline
		done 
	with
		End_of_file -> print_endline "\nGood bye."