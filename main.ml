Random.self_init () ;;

let service in_chan out_chan =
  let jeu = Jeu.creer_jeu_vide () in
  let continuer = ref true in
  let ecraser v_i v_j =
    Jeu.ecraser_vers jeu v_i v_j ;
    Jeu.ajouter_nombre jeu (if Random.int 5 = 0 then 4 else 2) (Random.int 32768)
  in
  let traiter str =
    if str = "haut" then
      ecraser (-1) 0
    else if str = "bas" then
      ecraser 1 0
    else if str = "gauche" then
      ecraser 0 (-1) 
    else if str = "droite" then
      ecraser 0 1
    else if str = "quitter" then
      (continuer := false ; true)
    else
      (output_string out_chan "Gnééé ?\n" ; true)
  in
  while !continuer do
    begin
      Jeu.output_game out_chan jeu ;
      output_string out_chan "\nhaut - bas - gauche - droite - quitter\n" ;
      flush out_chan ;
      try
	if not (traiter (String.trim (input_line in_chan))) then 
	  begin
	    output_string out_chan "\n Vous avez perdu !" ;
	    flush out_chan ;
	    continuer := false
	  end
      with
      | _ -> continuer := false
    end
  done 
;;

Unix.establish_server (service) (Unix.ADDR_INET(Unix.inet_addr_any, 45678)) ;;
