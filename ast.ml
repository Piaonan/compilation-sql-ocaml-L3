open Value 
open Relation
open Env

module R =
struct

  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)

  (* Fonctions d'agrégation (à compléter...) *)
  let sum dist =
    fun a r -> fold dist (fun acc v -> match acc with None -> Some v | Some v' -> Some (Value.add v' v)) None a r

end
type bonus = 
	| Normal of query
	| Open of string * string
	
and query = 
	Query of bool * projection * source * (condition option)
	| Union of query * query * bool
	| Except of query * query * bool
	| Intersect of query * query * bool
	
and projection =
	Asterisk
	| Column of (expression * string option) list
	(* expression As id *)
	
and source = 
	SrcID of string
	| SimpleQuery of query
	| SimpleSet of source * source
	| CrossJoin of source * source
	| Join of string * source * source * condition
	| LeftJoin of string * source * source * condition
	| RightJoin of string * source * source * condition
	| FullJoin of string * source * source * condition
	| NatJoin of string * source * source
	| NatLeftJoin of string * source * source
	| NatRightJoin of string * source * source
	| NatFullJoin of string * source * source

and condition = 
	Not of condition
	| And of condition * condition
	| Or of condition * condition
	| IsTrueOrNot of bool * condition
	| IsFalseOrNot of bool * condition
	| IsUnknownOrNot of bool * condition
	| Eq of expression * expression (*=*)
	| Neq of expression * expression (*<>*)
	| Lt of expression * expression (*<*)
	| Gt of expression * expression (*>*)
	| Le of expression * expression (*<=*)
	| Ge of expression * expression (*>=*)
	| BtwnOrNot of expression * bool * expression * expression
	| IsNullOrNot of bool * expression
	| CPar of condition
	
and expression = 
	Attribute of string * string
	| ExprInt of int
	| ExprFlo of float
	| ExprStr of string
	| ExprPlus of expression * expression
	| ExprMinus of expression * expression
	| ExprSlash of expression * expression
	| ExprAsterisk of expression * expression
	| ExprUminus of expression
	(* POUR LES STRING *)
	| Pipe of expression * expression
	| Lower of expression
	| Upper of expression
	| SubString of expression * expression * expression (* 0->n & inclus *)
	| CaseOne of (expression * (expression*expression)list * expression option) 
	| CaseTwo of ((condition*expression)list * expression option)
	| EPar of expression
	| Current
	| Date of expression
	| Extract of string * expression

	
let rec string_of_expr ex = match ex with
	| Attribute (s1, s2) -> s1 ^ "." ^ s2
	| ExprInt i -> Printf.sprintf "%d" i
	| ExprFlo f -> Printf.sprintf "%g" f
	| ExprStr s -> s
	| ExprPlus (e1, e2) -> Printf.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
	| ExprMinus (e1, e2) -> Printf.sprintf "%s - %s" (string_of_expr e1) (string_of_expr e2)
	| ExprSlash (e1, e2) -> Printf.sprintf "%s / %s" (string_of_expr e1) (string_of_expr e2)
	| ExprAsterisk (e1, e2) -> Printf.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
	| ExprUminus e -> Printf.sprintf "-%s" (string_of_expr e)
	| Pipe (e1, e2) -> Printf.sprintf "%s || %s" (string_of_expr e1) (string_of_expr e2)
	| Lower e -> Printf.sprintf "LOWER(%s)" (string_of_expr e)
	| Upper e -> Printf.sprintf "UPPER(%s)" (string_of_expr e)
	| SubString (e1,e2,e3) ->
		Printf.sprintf "SUBSTRING (%s FROM %s FOR %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
	| CaseOne (quand, cas, sinon) -> let rec jean l = (
		match l with
		| [] -> "\n" ^ ( match sinon with
			| None -> ""
			| Some e -> Printf.sprintf "ELSE %s" (string_of_expr e) )
		| (w,t) :: tail -> 
			Printf.sprintf 
				"\n  WHEN %s THEN %s%s" (string_of_expr w) (string_of_expr t) (jean tail)
		)
		in Printf.sprintf "CASE %s%s END" (string_of_expr quand) (jean cas)
	| CaseTwo (cas, sinon) -> let rec jean l = (
		match l with
		| [] -> "\n" ^ ( match sinon with
			| None -> ""
			| Some e -> Printf.sprintf "ELSE %s" (string_of_expr e) )
		| (w,t) :: tail -> 
			Printf.sprintf 
				"\n  WHEN %s THEN %s%s" (string_of_cond w) (string_of_expr t) (jean tail)
		) 
		in Printf.sprintf "CASE %s END" (jean cas)
	| EPar e -> Printf.sprintf "(%s)" (string_of_expr e)
	| Current -> "CURRENT_DATE"
	| Date e -> Printf.sprintf "DATE %s" (string_of_expr e)
	| Extract (s, e) -> Printf.sprintf "EXTRACT (%s FROM %s)" s (string_of_expr e)
		
and string_of_cond cond = match cond with
	| Not c -> Printf.sprintf "NOT %s" (string_of_cond c)
	| And (c1, c2) -> Printf.sprintf "%s AND %s" (string_of_cond c1) (string_of_cond c2)
	| Or (c1, c2) -> Printf.sprintf "%s OR %s" (string_of_cond c1) (string_of_cond c2)
	| IsTrueOrNot (b, c) -> let str = (string_of_cond c) in
		if b then Printf.sprintf "%s IS TRUE" str
		else Printf.sprintf "%s IS NOT TRUE" str
	| IsFalseOrNot (b, c) -> let str = (string_of_cond c) in
		if b then Printf.sprintf "%s IS FALSE" str
		else Printf.sprintf "%s IS NOT FALSE" str
	| IsUnknownOrNot (b, c) -> let str = (string_of_cond c) in
		if b then Printf.sprintf "%s IS UNKNOWN" str
		else Printf.sprintf "%s IS NOT UNKNOWN" str
	| Eq (e1, e2) -> Printf.sprintf "%s = %s" (string_of_expr e1) (string_of_expr e2)
	| Neq (e1, e2) -> Printf.sprintf "%s <> %s" (string_of_expr e1) (string_of_expr e2)
	| Lt (e1, e2) -> Printf.sprintf "%s < %s" (string_of_expr e1) (string_of_expr e2)
	| Gt (e1, e2) -> Printf.sprintf "%s > %s" (string_of_expr e1) (string_of_expr e2)
	| Le (e1, e2) -> Printf.sprintf "%s <= %s" (string_of_expr e1) (string_of_expr e2)
	| Ge (e1, e2) -> Printf.sprintf "%s >= %s" (string_of_expr e1) (string_of_expr e2)
	| BtwnOrNot (e1, b, e2, e3) -> let str = (string_of_expr e1) in
		if b then 
			Printf.sprintf "%s BETWEEN %s AND %s" str (string_of_expr e2) (string_of_expr e3)
		else Printf.sprintf "%s NOT BETWEEN %s AND %s" str (string_of_expr e2) (string_of_expr e3)
	| IsNullOrNot (b,e) -> let str = (string_of_expr e) in
		if b then Printf.sprintf "%s IS NULL" str
		else Printf.sprintf "%s IS NOT NULL" str
	| CPar c -> Printf.sprintf "(%s)" (string_of_cond c)

let string_of_proj pr = match pr with
	| Asterisk -> "*"
	| Column l -> ( let rec jean set = ( 
		match set with
		| [] -> ""
		| (e,o) :: [] -> (
			match o with
			| None -> Printf.sprintf "%s" (string_of_expr e)
			| Some s -> Printf.sprintf "%s AS %s" (string_of_expr e) s
			)
		| (e,o) :: tail -> (
			match o with
			| None -> Printf.sprintf "%s, %s" (string_of_expr e) (jean tail)
			| Some s -> Printf.sprintf "%s AS %s, %s" (string_of_expr e) s (jean tail)
			)
		)
		in jean l
	)

let okabe b = if b then "ALL " else ""
	
let rec string_of_src src = match src with
	| SrcID str -> str
	| SimpleQuery q -> Printf.sprintf "(%s)" (string_of_query q)
	| SimpleSet (s1, s2) -> Printf.sprintf "%s, %s" (string_of_src s1) (string_of_src s2)
	| CrossJoin (s1, s2) -> Printf.sprintf "%s CROSS JOIN %s" (string_of_src s1) (string_of_src s2)
	| Join (str, s1, s2, c) -> 
		Printf.sprintf "%s %sJOIN %s ON %s" (string_of_src s1) str (string_of_src s2) (string_of_cond c)
	| LeftJoin (str, s1, s2, c) -> 
		Printf.sprintf "%s LEFT %sJOIN %s ON %s" (string_of_src s1) str (string_of_src s2) (string_of_cond c)
	| RightJoin (str, s1, s2, c) -> 
		Printf.sprintf "%s RIGHT %sJOIN %s ON %s" (string_of_src s1) str (string_of_src s2) (string_of_cond c)
	| FullJoin (str, s1, s2, c) -> 
		Printf.sprintf "%s FULL %sJOIN %s ON %s" (string_of_src s1) str (string_of_src s2) (string_of_cond c)
	| NatJoin (str, s1, s2) -> 
		Printf.sprintf "%s NATURAL %sJOIN %s" (string_of_src s1) str (string_of_src s2)
	| NatLeftJoin (str, s1, s2) -> 
		Printf.sprintf "%s NATURAL LEFT %sJOIN %s" (string_of_src s1) str (string_of_src s2)
	| NatRightJoin (str, s1, s2) -> 
		Printf.sprintf "%s NATURAL RIGHT %sJOIN %s" (string_of_src s1) str (string_of_src s2)
	| NatFullJoin (str, s1, s2) -> 
		Printf.sprintf "%s NATURAL FULL %sJOIN %s" (string_of_src s1) str (string_of_src s2)
		
and string_of_query q = match q with
	| Query (b, pr, src, opt) -> (  
		let str = if b then "" else "DISTINCT " in
		match opt with
		| None -> 
			Printf.sprintf 
				"SELECT %s%s \nFROM %s" str (string_of_proj pr) (string_of_src src)
		| Some w -> 
			Printf.sprintf 
				"SELECT %s%s \nFROM %s \nWHERE %s" str (string_of_proj pr) (string_of_src src) (string_of_cond w)
	)
	| Union (q1, q2, b) ->
		Printf.sprintf "%s\nUNION %s\n%s" (string_of_query q1) (okabe b) (string_of_query q2)
	| Except (q1, q2, b) ->
		Printf.sprintf "%s\nEXCEPT %s\n%s" (string_of_query q1) (okabe b) (string_of_query q2)
	| Intersect (q1, q2, b) ->
		Printf.sprintf "%s\nINTERSECT %s\n%s" (string_of_query q1) (okabe b) (string_of_query q2)
				
	

	
let abc str = List.map (fun (a,b) -> (str^a,b))
				
let openTable nTable nFile gammGra = 
	let att, table = R.from_file (nTable^"."^nFile) '|' in
	Env.add nTable (table, (abc (nTable^".")) att) gammGra
				
	
let vin_att, vin = R.from_file "vin.csv" '|'
let viti_att, viticulteur = R.from_file "viticulteur.csv" '|'
let client_att, client = R.from_file "client.csv" '|'
let commande_att, commande = R.from_file "commande.csv" '|'


let commencement gammGra = let a = Env.add "vin" (vin, (abc "vin.") vin_att) gammGra in
	let b = Env.add "viticulteur" (viticulteur, (abc "viticulteur.") viti_att) a in
		let c = Env.add "client" (client, (abc "client.") client_att) b in
			Env.add "commande" (commande, (abc "commande.") commande_att) c

			
(* MEILLEURE VERSION DE LIST.ASSOC *)			
let monAssoc str l = let rec loop liste doc = match liste with
	| [] -> failwith ("La clef "^str^" n'existe pas :\n" ^ doc )
	| (a,b) :: t -> let ndoc = doc ^ "\n Clef : " ^ a in
					if str=a then b
					else loop t ndoc
	in loop l ""

let rec eval_expr (gammPet : R.attribute Env.env) e tup = match e with
	| Attribute (s1, s2) -> R.attribute (monAssoc (s1^"."^s2) gammPet) tup
	| ExprInt i -> Some (VInt i)
	| ExprFlo f -> Some (VFloat f)
	| ExprStr s -> Some (VVChar s)
	| ExprPlus (e1, e2) -> 
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		( match val1, val2 with
			| Some v1, Some v2 -> Some (Value.add v1 v2)
			| _ -> None )
	| ExprMinus (e1, e2) -> 
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		( match val1, val2 with
			| Some v1, Some v2 -> Some (Value.minus v1 v2) 
			| _ -> None )
	| ExprSlash (e1, e2) -> 
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		( match val1, val2 with
			| Some v1, Some v2 -> Some (Value.slash v1 v2) 
			| _ -> None )
	| ExprAsterisk (e1, e2) -> 
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		( match val1, val2 with
			| Some v1, Some v2 -> Some (Value.asterisk v1 v2) 
			| _ -> None )
	| ExprUminus e -> ( match eval_expr gammPet e tup with 
		| None -> None
		| Some v -> Some (Value.uminus v) )
	| Pipe (e1, e2) -> 
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		( match val1, val2 with
			| Some v1, Some v2 -> Some (Value.concat v1 v2) 
			| _ -> None )
	| Lower e -> ( match eval_expr gammPet e tup with 
		| None -> None
		| Some v -> Some (Value.lower v) )
	| Upper e -> ( match eval_expr gammPet e tup with 
		| None -> None
		| Some v -> Some (Value.upper v) )
	| SubString (e1,e2,e3) ->
		let val1 = eval_expr gammPet e1 tup in
		let val2 = eval_expr gammPet e2 tup in
		let val3 = eval_expr gammPet e3 tup in 
		( match val1, val2, val3 with
			| None, _, _ | _, None, _ | _, _, None  -> None
			| Some v1, Some v2, Some v3 -> Some (Value.substr v1 v2 v3) )
	| CaseOne (quand, cas, sinon) -> 
		let truc = (eval_expr gammPet quand tup) in
			let rec jean l = ( match l with 
				| [] -> ( match sinon with 
					| None -> None 
					| Some e -> eval_expr gammPet e tup )
				| (w,t) :: tail ->
					( match  Value.eq truc (eval_expr gammPet w tup) with
					| Some b -> if b then eval_expr gammPet t tup
								else jean tail
					| None -> jean tail )
			) in jean cas
	| CaseTwo (cas, sinon) -> 
		let rec jean l = ( match l with
			| [] -> ( match sinon with 
				| None -> None
				| Some e -> eval_expr gammPet e tup )
			| (w,t) :: tail ->
				if eval_cond gammPet w tup then eval_expr gammPet t tup
				else jean tail
		) in jean cas
	| EPar e -> eval_expr gammPet e tup
	| Current -> Some (Value.now())
	| Date e -> ( match eval_expr gammPet e tup with
		| None -> None
		| Some v -> Value.dateFormat v ) 
	| Extract (s,e) -> ( match eval_expr gammPet e tup with
		| None -> None
		| Some v -> (Value.extract s v) )
			
and fake_eval_cond gammPet cond tup = match cond with 
	| Not c -> 
		( match fake_eval_cond gammPet c tup with 
			| Some x -> Some (not x)
			| _ -> None )
	| And (c1, c2) -> 
		( match fake_eval_cond gammPet c1 tup, fake_eval_cond gammPet c2 tup with
			| Some x1, Some x2 -> Some (x1 && x2)
			| Some x, None | None, Some x -> if x then None else Some false
			| _ -> None )
	| Or (c1, c2) -> 
		( match fake_eval_cond gammPet c1 tup, fake_eval_cond gammPet c2 tup with 
			| Some x1, Some x2 -> Some (x1 || x2)
			| Some x, None | None, Some x -> if x then Some true else None
			| _ -> None )
	| IsTrueOrNot (b, c) -> 
		( match fake_eval_cond gammPet c tup with
			| Some x -> if b then Some (x = true) else Some (x = false)
			| None -> if b then Some false else Some true )
	| IsFalseOrNot (b, c) -> 
		( match fake_eval_cond gammPet c tup with
			| Some x -> if b then Some (x = false) else Some (x = true)
			| None -> if b then Some false else Some true )
	| IsUnknownOrNot (b, c) -> 
		( match fake_eval_cond gammPet c tup with
			| None -> if b then Some true else Some false
			| _ -> if b then Some false else Some true )
	| Eq (e1, e2) -> 
		Value.eq (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| Neq (e1, e2) -> 
		Value.neq (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| Lt (e1, e2) ->
		Value.lt (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| Gt (e1, e2) -> 
		Value.gt (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| Le (e1, e2) ->
		Value.le (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| Ge (e1, e2) -> 
		Value.ge (eval_expr gammPet e1 tup) (eval_expr gammPet e2 tup)
	| BtwnOrNot (e1, b, e2, e3) ->
		let val1 = (eval_expr gammPet e1 tup) in
		let val2 = (eval_expr gammPet e2 tup) in
		let val3 = (eval_expr gammPet e3 tup) in
		if b then 
			( match (Value.ge val1 val2), (Value.le val1 val3) with
			| Some b1, Some b2 -> Some (b1 && b2)
			| _ -> None )
		else 
			( match (Value.gt val1 val3), (Value.lt val1 val2) with
			| Some b1, Some b2 -> Some (b1 || b2)
			| _ -> None )
	| IsNullOrNot (b,e) -> 
		( match eval_expr gammPet e tup with
			| None -> if b then Some true else Some false
			| _ -> if b then Some false else Some true )
	| CPar c -> fake_eval_cond gammPet c tup
			
and eval_cond gammPet cond tup = 
	match fake_eval_cond gammPet cond tup with
	| None -> false
	| Some b -> b
	
let eval_cond_for_join gammPet cond tup1 tup2 = 
	eval_cond gammPet cond (R.daoko tup1 tup2)
	
let harmonie gamm1 gamm2 = 
	Env.union gamm1 (List.map (fun (a,b) -> (a, b + (List.length gamm1))) gamm2)
	
	
(* ON PREND LE SUFFIXE DU MOT STR APRES LE CARACTERE C *)	
let sous str = 
	Value.down str '.'
(* ON PREND LE PREFIXE DU MOT STR AVANT LE CARACTERE C *)
let sur str = 
	Value.top str '.'
(* ON TRIE GAMMA EN FONCTION DE LA VALEUR ET NON DE LA CLEF POUR RANGER LES ATTRIBUTS LORS DE LA NATURAL JOIN *)
let tri gamm = 
	let rec loop l result = 
	match l with
		| [] -> result
		| (str,v) :: tail -> loop tail (Env.sort str v result)
	in loop gamm []
	
let discordance gamm1 gamm2 gap = (* ON NE PREND PAS LES ATTRIBUTS EN COMMUN *)
	let rec loop l perte =
	match l with
		| [] -> []
		| (str,v)::tail -> 
			if List.exists (fun (a,b) -> (sous str)=(sous a)) gamm1
			then loop tail (perte+gap)
			else (str,v-perte) :: (loop tail perte)
	in harmonie gamm1 (loop (tri gamm2) 0) 
let concordance gamm =	(* ON PREND LES ATTRIBUTS EN COMMUN *)
	let rec loop l =
	match l with
		| [] -> []
		| (str,v)::tail -> 
			if List.exists (fun (a,b) -> (sous str)=(sous a)) tail 
			then (sous str,v) :: (loop tail)
			else loop tail
	in loop gamm
let sorting gamm1 gamm2 = (* ON TRIE LES ATTRIBUTS POUR LA PROJECTION *)
	List.map (fun (a)-> (DVChar, R.attribute a)) 
	(List.sort compare (
	(List.map (fun (a,b) -> b) 
	(discordance gamm1 gamm2 0)
	) ) )

(* ON CREE DES ATTRIBUTS POUR FAIRE LA CONDITION DE LA NATURAL JOIN *)
let fake_attribute gamm str =
	let rec loop l = match l with
	| [] -> failwith "Erreur impossible fake_attribute"
	| (a,_) :: tail -> 
		if (sous a)=str 
			then Attribute (sur a, str) 
		else loop tail
	in loop gamm
(* ON CREE UNE CONDITION QUI LIE LES ATTRIBUTS EN COMMUM POUR LA NATURAL JOIN *)
let create_cond_for_nat concorde gamm1 gamm2 =
	let rec fake_cond l = match l with
		| [] -> failwith "Erreur impossible fake_cond"
		| (str,v)::[] -> Eq (fake_attribute gamm1 str, fake_attribute gamm2 str)
		| (str,v)::tail -> And (Eq (fake_attribute gamm1 str, fake_attribute gamm2 str), fake_cond tail)
	in eval_cond_for_join (harmonie gamm1 gamm2) (fake_cond concorde)
	
let allOrDistinct b r = if b then r else R.distinct r	

let rec factJoin fct gammGra s1 s2 c = 
	let src1 = eval_src gammGra s1 in
	let src2 = eval_src gammGra s2 in 
	( match src1, src2 with
		(r1, gammPet1), (r2, gammPet2) -> 
	let gammPet = harmonie gammPet1 gammPet2 in
	let tutu = eval_cond_for_join gammPet c in
	(fct tutu r1 r2, gammPet) )

and factNatJoin fct gammGra s1 s2 = 
	let src1 = eval_src gammGra s1 in
	let src2 = eval_src gammGra s2 in 
	( match src1, src2 with 
		(r1, gammPet1), (r2, gammPet2) -> 
	let gammPet = harmonie gammPet1 gammPet2 in
	let concorde = concordance gammPet in
	if (List.length concorde)=0
		then (R.crossjoin r1 r2, gammPet)
	else
		let discorde = discordance gammPet1 gammPet2 1 in
		let tutu = create_cond_for_nat concorde gammPet1 gammPet2 in
		(R.projection (sorting gammPet1 gammPet2) (fct tutu r1 r2), discorde)
	)
	
and eval_src gammGra src = match src with
	| SrcID str -> ( match find str gammGra with 
		| Some truc -> truc
		| None -> failwith ("Il n'y a pas la table " ^ str) )
	| SimpleQuery q -> eval_query gammGra q
	| SimpleSet (s1, s2) 
	| CrossJoin (s1, s2) -> let src1 = eval_src gammGra s1 in
		let src2 = eval_src gammGra s2 in 
			( match src1, src2 with
				(r1, gammPet1), (r2, gammPet2) ->
				(R.crossjoin r1 r2, harmonie gammPet1 gammPet2) )
	| Join (_, s1, s2, c) -> 
		factJoin (R.innerjoin) gammGra s1 s2 c
	| LeftJoin (_, s1, s2, c) -> 
		factJoin (R.leftouterjoin) gammGra s1 s2 c
	| RightJoin (_, s1, s2, c) ->
		factJoin (R.rightouterjoin) gammGra s1 s2 c
	| FullJoin (_, s1, s2, c) -> 
		factJoin (R.fullouterjoin) gammGra s1 s2 c
	| NatJoin (_, s1, s2) -> 
		factNatJoin (R.innerjoin) gammGra s1 s2
	| NatLeftJoin (_, s1, s2) -> 
		factNatJoin (R.leftouterjoin) gammGra s1 s2
	| NatRightJoin (_, s1, s2) -> 
		factNatJoin (R.rightouterjoin) gammGra s1 s2
	| NatFullJoin (_, s1, s2) -> 
		factNatJoin (R.fullouterjoin) gammGra s1 s2
				
and eval_query gammGra q = match q with
	| Query (b, pr, src, opt) -> (
		if b then
			( match pr with
				| Asterisk -> ( match opt with 
					| None -> eval_src gammGra src
					| Some cond -> ( match eval_src gammGra src with
						| (r, gammPet) -> (R.selection (eval_cond gammPet cond) r, gammPet) ) )
				| Column projList -> 
					let r, gammPet = eval_query gammGra (Query (true, Asterisk, src, opt)) in 
					let nouveauGamm, proj = eval_proj gammPet (List.rev projList) in 
						(R.projection proj r, nouveauGamm)	
			)
		else 
			( match eval_query gammGra (Query (true, pr, src, opt)) with
			| (r, gammPet) -> (R.distinct r, gammPet) )
	)
	| Union (q1, q2, b) -> (
		match eval_query gammGra q1, eval_query gammGra q2 with
			| (r1, gammPet1), (r2, gammPet2) -> 
				(allOrDistinct b (R.union r1 r2), gammPet1)
	)
	| Except (q1, q2, b) -> (
		match eval_query gammGra q1, eval_query gammGra q2 with
			| (r1, gammPet1), (r2, gammPet2) -> 
				(allOrDistinct b (R.diff r1 r2), gammPet1)
	)
	| Intersect (q1, q2, b) -> (
		match eval_query gammGra q1, eval_query gammGra q2 with
			| (r1, gammPet1), (r2, gammPet2) -> 
				(allOrDistinct b (R.inter r1 r2), gammPet1)
	)

and eval_proj gammPet pr = match pr with
	| [] -> failwith "Projection anormale"
	| (e,opt) :: [] -> let actual = ((DVChar, (eval_expr gammPet e))::[]) in
		( match opt with
		| None -> (Env.add "" (0 : R.attribute) empty, actual) 
		| Some id -> (Env.add id (0 : R.attribute) empty, actual) )
	| (e,opt) :: tail -> let actual = ((DVChar, (eval_expr gammPet e))::[]) in
		( match eval_proj gammPet tail with
		| (nouveauGamm, proj) -> ( match opt with 
			| None -> (Env.add "" (List.length nouveauGamm) nouveauGamm, List.append proj actual)
			| Some id -> (Env.add id (List.length nouveauGamm) nouveauGamm, List.append proj actual) 
			) 
		)


	

	
let newFirstQuery p s = Query (true, p, s, None)
let newDistinctFirstQuery p s = Query (false, p, s, None)
let newSecondQuery p s c = Query (true, p, s, Some c)
let newDistinctSecondQuery p s c = Query (false, p, s, Some c)
let newUnion q1 q2 = Union (q1, q2, false)
let newUnionAll q1 q2 = Union (q1, q2, true)
let newExcept q1 q2 = Except (q1, q2, false)
let newExceptAll q1 q2 = Except (q1, q2, true)
let newIntersect q1 q2 = Intersect (q1, q2, false)
let newIntersectAll q1 q2 = Intersect (q1, q2, true)

let newAsterisk = Asterisk
let newColumn e = Column ((e, None)::[])
let newColumnAs e s = Column ((e,Some (s))::[])
let newColCoCol p1 p2 = match p1, p2 with
	| Column l1, Column l2 -> Column (List.append l1 l2)
	| _ -> failwith "Erreur impossible"

let newSrcID s = SrcID s
let newSimpleQuery q = SimpleQuery q
let newSimpleSet s1 s2 = SimpleSet (s1,s2)
let	newCrossJoin s1 s2 = CrossJoin (s1,s2)
let newJoin s1 s2 c = Join ("", s1, s2, c)
let newLeftJoin s1 s2 c = LeftJoin ("", s1, s2, c)
let newRightJoin s1 s2 c = RightJoin ("", s1, s2, c)
let newFullJoin s1 s2 c = FullJoin ("", s1, s2, c)
let newInnerJoin s1 s2 c = Join ("INNER ", s1, s2, c)
let newLeftOuterJoin s1 s2 c = LeftJoin ("OUTER ", s1, s2, c)
let newRightOuterJoin s1 s2 c = RightJoin ("OUTER ", s1, s2, c)
let newFullOuterJoin s1 s2 c = FullJoin ("OUTER ", s1, s2, c)
let newNatJoin s1 s2 = NatJoin ("", s1, s2)
let newNatLeftJoin s1 s2 = NatLeftJoin ("", s1, s2)
let newNatRightJoin s1 s2 = NatRightJoin ("", s1, s2)
let newNatFullJoin s1 s2 = NatFullJoin ("", s1, s2)
let newNatInnerJoin s1 s2 = NatJoin ("INNER ", s1, s2)
let newNatLeftOuterJoin s1 s2 = NatLeftJoin ("OUTER ", s1, s2)
let newNatRightOuterJoin s1 s2 = NatRightJoin ("OUTER ", s1, s2)
let newNatFullOuterJoin s1 s2 = NatFullJoin ("OUTER ", s1, s2)

let newNot c = Not c
let newAnd c1 c2 = And (c1, c2)
let newOr c1 c2 = Or (c1, c2)
let newIsTrue c = IsTrueOrNot (true,c)
let newIsNotTrue c = IsTrueOrNot (false,c)
let newIsFalse c = IsFalseOrNot (true,c)
let newIsNotFalse c = IsFalseOrNot (false,c)
let newIsUnknown c = IsUnknownOrNot(true,c)
let newIsNotUnknown c = IsUnknownOrNot(false,c)
let newEq e1 e2 = Eq(e1,e2)
let newNeq e1 e2 = Neq(e1,e2)
let newLt e1 e2 = Lt(e1,e2)
let newGt e1 e2 = Gt(e1,e2)
let newLe e1 e2 = Le(e1,e2)
let newGe e1 e2 = Ge(e1,e2)
let newBtwn e1 e2 e3 = BtwnOrNot (e1,true,e2,e3)
let newNotBtwn e1 e2 e3 = BtwnOrNot (e1,false,e2,e3)
let newIsNull e = IsNullOrNot(true,e)
let newIsNotNull e = IsNullOrNot(false,e)
let newCPar c = CPar c

let newAttribute s1 s2 = Attribute(s1, s2)
let newExprInt i = ExprInt i
let newExprFlo f = ExprFlo f
let newExprStr s = ExprStr s 
let newExprPlus e1 e2 = ExprPlus(e1,e2) 
let newExprMinus e1 e2 = ExprMinus(e1,e2)
let newExprSlash e1 e2 = ExprSlash(e1,e2)
let newExprAsterisk e1 e2 = ExprAsterisk(e1,e2) 
let newExprUminus e = ExprUminus e
let newPipe e1 e2 = Pipe(e1,e2)
let newLower e = Lower e
let newUpper e = Upper e
let newSubString e1 e2 e3 = SubString(e1,e2,e3)
let newCaseOne e1 l e2 = CaseOne (e1, l, Some e2)
let newCaseOneBis e l = CaseOne (e, l, None)
let newCaseTwo l e = CaseTwo (l, Some e)
let newCaseTwoBis l = CaseTwo (l, None)
let newEPar e = EPar e
let newDate e = Date e
let newCurrent = Current
let newExtract s e = Extract (s, e)


let eval_simpleQuery gammGra q = let _ = match eval_query gammGra q with
		| (r, attrib) -> R.print '|' (List.map (fun (a,b) -> (b,a)) attrib) r
		in print_newline ()
		
(* OUVERTURE DE FICHIER FACILE *)
let newOpen s1 s2 = Open (s1,s2)
let newNormal q = Normal q
let string_of_bonus bonus =
	( match bonus with
	| Open (s1,s2) -> Printf.sprintf "OPEN %s.%s" s1 s2
	| Normal q -> Printf.sprintf "%s" (string_of_query q) )
	
let eval_bonus gammGra bonus = 
	( match bonus with
	| Open (s1,s2) -> openTable s1 s2 gammGra
	| Normal q -> let _ = eval_simpleQuery gammGra q in gammGra )
	









		

	
	
	
	
	
