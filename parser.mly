/* File parser.mly */
%{
	open Core

	let check_equal s =
		if s<>"=" then failwith "not equal"

	let rec popTail =
		function
		| [] -> failwith "no tail"
		| a::[] -> ([], a)
		| a::b -> let c,d = popTail b in (a::c, d)

%}
%token <string> CMD
%token <string> LOW_OPERATOR
%token <string> HIGH_OPERATOR
%token <string> LOG_OPERATOR
%token <string> VARIABLE
%token <int> INT 
%token <float> DEC
%token <bool> BOOL
%token PIPE LBRACKET RBRACKET LPAREN RPAREN EOL
%token INPUT OUTPUT
%token WHERE FIELDS EVAL COMMA JOIN
%left LOW_OPERATOR
%left HIGH_OPERATOR
%left LOG_OPERATOR
%start main             /* the entry point */
%type <Core.query> main
%%
main:  
  | query EOL	          	  	  { ($1:Core.query) }
  | EOL							  { ([]:Core.query) }

query: 
  | LBRACKET query RBRACKET    	  { $2 }
  | command PIPE query            { $1 :: $3 }
  | command EOL PIPE query        { $1 :: $4 }
  | command EOL 		          { $1 :: [] }

varlist:
  | VARIABLE varlist 		{ $1::$2 }
  | VARIABLE COMMA varlist  { $1::$3 }
  | VARIABLE				{ $1 :: [] }

evalSeq:
  | eval COMMA evalSeq 	{ $1::$3 }
  | eval 				{ $1 :: [] }

eval:
  | eval HIGH_OPERATOR eval 		{ Function ($2, [$1; $3]) }
  | eval LOW_OPERATOR eval 			{ Function ($2, [$1; $3]) }
  | eval LOG_OPERATOR eval 			{ Function ($2, [$1; $3]) }
  | VARIABLE LPAREN evalSeq RPAREN  { Function ($1, $3)}
  | VARIABLE 						{ Var $1 }
  | INT  							{ Int $1 }
  | DEC								{ Float $1 }
  | BOOL          					{ Bool $1 }

evalStatement:
  | VARIABLE LOG_OPERATOR eval { check_equal $2; ($1,$3) }

evalList: 
  | evalStatement evalList { $1::$2 }
  | evalStatement { $1 :: [] }

conditionList:
  | eval conditionList { $1::$2 }
  | eval { $1 :: [] }

command:
  | INPUT varlist { Input $2 }
  | OUTPUT VARIABLE { Output $2 }
  | WHERE conditionList { Where $2 }
  | EVAL evalList { Eval $2 }
  | FIELDS varlist { Fields (true, $2) }
  | FIELDS LOW_OPERATOR varlist { 
  		let add = match $2 with
  			  "+" -> true
  			| "-" -> false
  			| _ -> failwith "wrong fields statement"
  		in Fields (add, $3) 
  	}
  | JOIN varlist { 
  		let vl,tail = popTail $2 in
  		Join (vl, [Input [tail]]) 
  	}
  | JOIN varlist LBRACKET query RBRACKET { Join ($2, $4) }




