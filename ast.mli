open Value 
open Relation
open Env
module R :
sig
  type relation = Relation.Make(Value).relation
  type attribute = int
end
(* Syntaxe abstraite *)
type bonus
and query 
and projection
and source
and condition
and expression

val commencement: (R.relation * R.attribute Env.env)Env.env -> (R.relation * R.attribute Env.env)Env.env

(* Conversion en chaîne de caractères pour affichage *)
val string_of_bonus: bonus -> string

(* Evaluateur *)
val eval_bonus: (R.relation * R.attribute Env.env)Env.env -> bonus -> (R.relation * R.attribute Env.env)Env.env


(* Constructeurs d'expression *)
val newFirstQuery: projection -> source -> query
val newDistinctFirstQuery: projection -> source -> query
val newSecondQuery: projection -> source -> condition -> query
val newDistinctSecondQuery: projection -> source -> condition -> query
val newUnion: query -> query -> query
val newUnionAll: query -> query -> query
val newExcept: query -> query -> query
val newExceptAll: query -> query -> query
val newIntersect: query -> query -> query
val newIntersectAll: query -> query -> query

val newAsterisk: projection
val newColumn: expression -> projection
val newColumnAs: expression -> string -> projection
val newColCoCol: projection -> projection -> projection

val newSrcID: string -> source
val newSimpleQuery: query -> source
val newSimpleSet: source -> source -> source
val	newCrossJoin: source -> source -> source
val newJoin: source -> source -> condition -> source
val newLeftJoin: source -> source -> condition -> source
val newRightJoin: source -> source -> condition -> source
val newFullJoin: source -> source -> condition -> source
val newInnerJoin: source -> source -> condition -> source
val newLeftOuterJoin: source -> source -> condition -> source
val newRightOuterJoin: source -> source -> condition -> source
val newFullOuterJoin: source -> source -> condition -> source
val newNatJoin: source -> source -> source
val newNatLeftJoin: source -> source -> source
val newNatRightJoin: source -> source -> source
val newNatFullJoin: source -> source -> source
val newNatInnerJoin: source -> source -> source
val newNatLeftOuterJoin: source -> source -> source
val newNatRightOuterJoin: source -> source -> source
val newNatFullOuterJoin: source -> source -> source

val newNot: condition -> condition
val newAnd: condition -> condition -> condition
val newOr: condition -> condition -> condition
val newIsTrue: condition -> condition
val newIsNotTrue: condition -> condition
val newIsFalse: condition -> condition
val newIsNotFalse: condition -> condition
val newIsUnknown: condition -> condition
val newIsNotUnknown: condition -> condition
val newEq: expression -> expression -> condition
val newNeq: expression -> expression -> condition
val newLt: expression -> expression -> condition
val newGt: expression -> expression -> condition
val newLe: expression -> expression -> condition
val newGe: expression -> expression -> condition
val newBtwn: expression -> expression -> expression -> condition
val newNotBtwn: expression -> expression -> expression -> condition
val newIsNull: expression -> condition
val newIsNotNull: expression -> condition
val newCPar: condition -> condition

val newAttribute: string -> string -> expression
val newExprInt: int -> expression
val newExprFlo: float -> expression
val newExprStr: string -> expression
val newExprPlus: expression -> expression -> expression
val newExprMinus: expression -> expression -> expression
val newExprSlash: expression -> expression -> expression
val newExprAsterisk: expression -> expression -> expression 
val newExprUminus: expression -> expression
val newPipe: expression -> expression -> expression
val newLower: expression -> expression
val newUpper: expression -> expression
val newSubString: expression -> expression -> expression -> expression
val newCaseOne: expression -> (expression * expression) list -> expression -> expression
val newCaseOneBis: expression -> (expression * expression) list -> expression 
val newCaseTwo: (condition * expression) list -> expression -> expression
val newCaseTwoBis: (condition * expression) list -> expression
val newEPar: expression -> expression
val newDate: expression -> expression
val newCurrent: expression
val newExtract: string -> expression -> expression

val newOpen: string -> string -> bonus
val newNormal: query -> bonus







