type variable = string

and evalExpr = 
	  Var of variable
	| Int of int
	| Float of float
	| Bool of bool
	| Function of variable*(evalExpr list)

and command =
	  Input of string list
	| Output of string
	| Where of (evalExpr list)
	| Fields of bool*(string list)
	| Eval of (variable*evalExpr) list
	| Join of (variable list)*query

and query = command list
	

let rec print_eval =
	function
	| Var x -> print_string x
	| Int x -> print_string (string_of_int x)
	| Float x -> print_string (string_of_float x)
	| Bool x -> print_string (string_of_bool x)
	| Function (f,args) ->
		Printf.printf "%s(" f;
		List.iter (fun arg ->
			print_eval arg;
			print_string ","
		) args; print_string ")"


and print_condition =
	function
	| _ -> print_string "condition"

and print_command =
	function 
	| Input l -> 
	  	print_string "-- input: ";
	  	List.iter (Printf.printf "%s ") l
	| Output s -> Printf.printf "-- output: %s\n" s
	| Eval l ->
		List.iter ( fun (var,evalExpr) ->
			Printf.printf "-- eval: %s=" var;
			print_eval evalExpr;
			print_newline ()
		) l
	| Where conditions ->
		List.iter ( fun cond ->
			print_string "-- where: ";
			print_eval cond;
			print_newline ()
		) conditions
	| Fields (add, varlist) ->
		Printf.printf "-- fields(%b): " add;
		List.iter ( fun var ->
			Printf.printf "%s " var
		) varlist
	| Join (varlist, query) -> 
		Printf.printf "-- join: ";
		List.iter ( fun var ->
			Printf.printf "%s " var
		) varlist;
		Printf.printf "[\n";
		print_query query;
		Printf.printf "]\n"

and print_query q =
	List.iter (fun c -> 
			print_command c;
			print_newline ()) q






