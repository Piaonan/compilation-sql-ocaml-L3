(* Module pour la représentation et la manipulation des valeurs atomiques
 *
 * Ce module respecte la signature Relation.DATA et peut donc être utilisé
 * en argument de Relation.Make
 *)


(* Définition des types relatant des domaines et des valeurs atomiques manipulables *)

type domain =
  | DInt
  | DFloat
  | DVChar

type value =
  | VInt   of int
  | VFloat of float
  | VVChar of string

(* Fonctions de conversion entre chaînes de caractères et valeurs/domaines (utilisées dans l'import/export des CSV) *)

let domain_of_string s =
  match s with
  | "INT" -> DInt
  | "FLOAT" -> DFloat
  | "VARCHAR" -> DVChar
  | _ -> failwith (Printf.sprintf "Value: domain_of_string: unknown domain: '%s'" s)

let string_of_domain d =
  match d with
  | DInt -> "INT"
  | DFloat -> "FLOAT"
  | DVChar -> "VARCHAR"

let value_of_string d =
  match d with
  | DInt -> (fun s -> VInt (int_of_string s))
  | DFloat -> (fun s -> VFloat (float_of_string s))
  | DVChar -> (fun s -> VVChar s)

let string_of_value v =
  match v with
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VVChar s -> s

(* Fonctions de conversion et de vérification d'appartenance d'une valeur à un domaine *)

let domain_of_value v =
  match v with
  | VInt _ -> DInt
  | VFloat _ -> DFloat
  | VVChar _ -> DVChar

let domain d v =
  match d, v with
  | DInt, VInt _
  | DFloat, VFloat _
  | DVChar, VVChar _ -> true
  | _ -> false

let to_domain d =
  match d with
  | DInt -> (function
    | VInt i -> VInt i
    | VFloat f -> VInt (int_of_float f)
    | VVChar s -> try VInt (int_of_string s) with Failure _ -> VInt 0
  )
  | DFloat -> (function
    | VInt i -> VFloat (float_of_int i)
    | VFloat f -> VFloat f
    | VVChar s -> try VFloat (float_of_string s) with Failure _ -> VFloat 0.
  )
  | DVChar -> (function
    | VInt i -> VVChar (string_of_int i)
    | VFloat f -> VVChar (string_of_float f)
    | VVChar s -> VVChar s
  )

(* Fonction spécifique de manipulation des valeurs (comparaison, addition, concaténation, etc.) *)
let fact forInt forFloat v1 v2 = match v1, v2 with
	| VInt i, VInt j -> VInt (forInt i j)
	| VFloat f, VFloat e -> VFloat (forFloat f e)
	| VFloat f, VInt i -> VFloat (forFloat f (float_of_int i))
	| VInt i, VFloat f -> VFloat (forFloat (float_of_int i) f)
	| _ -> failwith 
		(Printf.sprintf "Value: operation: type error: '%s ['+' '-' '*' '/'] %s'" (string_of_value v1) (string_of_value v2))

let add v1 v2 = fact (+) (+.) v1 v2
let minus v1 v2 = fact (-) (-.) v1 v2
let asterisk v1 v2 = fact ( * ) ( *. ) v1 v2
let slash v1 v2 = fact (/) (/.) v1 v2
let uminus v = match v with
	| VInt i -> VInt (-i)
	| VFloat f -> VFloat (-.f)
	| _ -> failwith 
		(Printf.sprintf "Value: Uminus: type error: '%s'" (string_of_value v))


let upper v = VVChar (String.uppercase (string_of_value v))
let lower v = VVChar (String.lowercase (string_of_value v))

let substr v1 v2 v3 = let s = string_of_value v1 in
	match v2, v3 with
		| VInt i, VInt j -> VVChar (String.sub s (i-1) j)
		| _ -> failwith (Printf.sprintf "Value: Substring: type error: '%s FROM %s FOR %s'" (string_of_value v1) (string_of_value v2) (string_of_value v3))	
	
	
let concat v1 v2 = VVChar ((string_of_value v1) ^ (string_of_value v2))
  
  
let fact2 fctI fctF fctS o1 o2 = match o1, o2 with
	| None, _ | _, None -> None
	| Some v1, Some v2 -> ( match v1, v2 with
		| VInt i, VInt j -> Some (fctI i j)
		| VFloat f, VFloat e -> Some (fctF f e)
		| VFloat f, VInt i -> Some (fctF f (float_of_int i))
		| VInt i, VFloat f -> Some (fctF (float_of_int i) f)
		| VVChar c1, VVChar c2 -> Some (fctS c1 c2)
		| a,b -> failwith (Printf.sprintf "error with fact for eval_cond %s & %s" (string_of_value a) (string_of_value b) ))
;;

let lt o1 o2 = fact2 (<) (<) (<) o1 o2
let gt o1 o2 = fact2 (>) (>) (>) o1 o2
let le o1 o2 = fact2 (<=) (<=) (<=) o1 o2
let ge o1 o2 = fact2 (>=) (>=) (>=) o1 o2
let eq o1 o2 = fact2 (=) (=) (=) o1 o2
let neq o1 o2 = fact2 (!=) (<>) (<>) o1 o2

(* MANIPULATION DU TEMPS *)
let down str c = 
	let i = (String.index str c) + 1 in
	String.sub str i ((String.length str) -i)
let top str c = 
	String.sub str 0 (String.index str c)
	
let authentique date = (* INSPECTEUR DE TEMPS *)
	let size = String.length date in
	let tank i = (String.get date i) in
	let inter i = (int_of_char (String.get date i)) - (int_of_char '0') in
	let rec loop i = 
		if i = size then true
		else if ((i=4||i=7) && ((tank i)='-')) 
			|| (i!=4 && i!=7 && (inter i)<=9 && (inter i)>=0)
			then loop (i+1)
		else false
	in (size = 10) && (loop 0)
	
let dateFormat v = (* INSPECTEUR DE TEMPS *)
	( match v with 
	| VVChar date -> 
		if authentique date then Some (VVChar date) 
		else None
	| _ -> None )
	
let now go = (* DONNEUR DE TEMPS *)
	let temps = Unix.gmtime(Unix.time go) in
	VVChar (Printf.sprintf "%.4d-%.2d-%.2d" 
	(temps.Unix.tm_year + 1900) (temps.Unix.tm_mon + 1) temps.Unix.tm_mday)
	
let extract field v = (* EXTRACTEUR DE TEMPS *)
	( match v with
	| VVChar date ->
		if (authentique date) then
			if field = "YEAR" then
				Some (VVChar (top date '-'))
			else if field = "MONTH" then
				Some (VVChar (top (down date '-') '-'))
			else if field = "DAY" then
				Some (VVChar (down (down date '-') '-'))
			else None
		else None
	| _ -> None )







