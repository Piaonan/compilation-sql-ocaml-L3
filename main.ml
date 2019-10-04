let gammGra = Ast.commencement Env.empty;;

let _ =

  (* Ouverture un flot de caractère ; ici à partir de l'entrée standard *)  
  let source = Lexing.from_channel stdin in

  (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
  let rec f gammGra () =
    try
      (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
      let quest = Parser.ansyn Lexer.anlex source in
      Printf.printf "\n-> (%s) <-\n\n" (Ast.string_of_bonus quest); 
	  let gammGra = (Ast.eval_bonus gammGra quest) in
	  flush stdout;
      f gammGra ()
    with Lexer.Eof -> Printf.printf "END\n"
  in

  f gammGra ()


	
	
	
	
	