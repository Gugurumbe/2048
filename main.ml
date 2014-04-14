Random.self_init () ;;

let service in_chan out_chan =
  let jeu = Jeu.creer_jeu_vide () in
  let continuer = ref true in
  let traiter str =
    if str = "haut" then
      Jeu.ecraser_vers jeu (-1) 0
    else if str = "bas" then
      Jeu.ecraser_vers jeu 1 0
    else if str = "gauche" then
      Jeu.ecraser_vers jeu 0 (-1) 
    else if str = "droite" then
      Jeu.ecraser_vers jeu 0 1
    else if str = "quitter" then
      continuer := true 
    else
      output_string "Gnééé ?\n"
  in
  while !continuer do
    Jeu.ajouter_nombre jeu (if Random.int 5 = 0 then 4 else 2) (Random.int max_int) ;
    Jeu.output_game out_chan jeu ;
    output_string "\nhaut - bas - gauche - droite - quitter\n" ;
    try
      traiter (String.trim (input_line in_chan))
    with
    | _ -> continuer := false
  done 
;;

Unix.establish_server (service) (Unix.INET_ADDR(Unix.inet_addr_any, 45678)) ;;
