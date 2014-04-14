type case = 
| Libre
| Nombre of int
;;

let creer_jeu_vide () = Array.init 4 (fun _ -> Array.make 4 Libre) ;;

let ajouter_nombre jeu k roulette =
  let positions_possibles = ref [] in
  let nombre_possibles = ref 0 in
  for i=0 to 3 do
    for j=0 to 3 do
      match jeu.(i).(j) with
      | Libre ->
	begin
	  positions_possibles := (i, j)::(!positions_possibles) ;
	  incr nombre_possibles
	end
      |_ -> ()
    done ;
  done ;
  let rec extraire i = function
    |[] -> failwith "bloups"
    |h::_ when i = 0 -> h
    |_::t -> extraire (i-1) t
  in
  if !nombre_possibles <> 0 then
    begin
      let (i, j) = extraire (roulette mod !nombre_possibles) (!positions_possibles) in
      jeu.(i).(j) <- Nombre(k) ;
    end ;
  !nombre_possibles <> 0
;;

let ecraser_vers jeu vect_i vect_j =
  let in_range i j = i >= 0 && j >= 0 && i < 4 && j < 4 in
  let pousser i j =
    (*print_endline ("Je pousse en ("^(string_of_int i)^", "^(string_of_int j)^").") ;*)
    match jeu.(i).(j) with
    | Libre -> 
      begin
	(*print_endline ("\tCase libre en ("^(string_of_int i)^", "^(string_of_int j)^") !") ;*)
	(*print_endline "Effectué." ;*)
      end
    | Nombre(k) when in_range (i + vect_i) (j + vect_j) ->
      begin
	match jeu.(i + vect_i).(j + vect_j) with
	| Libre ->
	  begin
	    (*print_endline ("\tNombre en ("^(string_of_int i)^", "^(string_of_int j)^") à déplacer en ("^(string_of_int (i+vect_i))^", "^(string_of_int (j+vect_j))^").") ;*)
	    jeu.(i+vect_i).(j+vect_j) <- jeu.(i).(j) ;
	    jeu.(i).(j) <- Libre ;
	    (*pousser (i+vect_j) (j+vect_j) ;*)
	    (*print_endline "Effectué." ;*)
	  end
	| Nombre(l) when l = k ->
	  begin
	    (*print_endline ("\tNombre en ("^(string_of_int i)^", "^(string_of_int j)^") à fusionner en ("^(string_of_int (i+vect_i))^", "^(string_of_int (j+vect_j))^").") ;*)
	    jeu.(i+vect_i).(j+vect_j) <- Nombre(2*k) ;
	    jeu.(i).(j) <- Libre ;
	    (*pousser (i+vect_i) (j+vect_j) ;*)
	    (*print_endline "Effectué." ;*)
	  end
	| _ -> ()
      end
    | _ -> 
      begin
	(*print_endline ("\tNombre en ("^(string_of_int i)^", "^(string_of_int j)^"), mais on est sur le bord.") ;*)
	(*print_endline "Effectué." ;*)
      end
  in
  for k=0 to 3 do (* max nb_lignes nb_colonnes*)
    for i=0 to 3 do
      for j=0 to 3 do 
      (*bof bof bof*)
	pousser i j
      done ;
    done 
  done 
;;

let output_game flux jeu =
  let table_contenus = Array.init 4 (fun i -> Array.init 4 (fun j ->
    match jeu.(i).(j) with
    | Libre -> ""
    | Nombre(k) -> string_of_int k
  )) in
  let max_taille = ref 0 in
  for i=0 to 3 do
    for j=0 to 3 do
      max_taille := max !max_taille (String.length table_contenus.(i).(j))
    done ;
  done ;
  let agrandir chaine =
    let nb_espaces_manquants = (!max_taille) - (String.length chaine) in
    let espaces_a_gauche = nb_espaces_manquants / 2 in
    let espaces_a_droite = nb_espaces_manquants - espaces_a_gauche in
    (String.make espaces_a_gauche ' ')^chaine^(String.make espaces_a_droite ' ')
  in
  let table = Array.map (Array.map (agrandir)) table_contenus in
  let ecrire_separation () =
    output_string flux "\n+" ;
    for i=0 to 3 do
      for k = 0 to -1 + !max_taille do
	output_string flux "-" ;
      done ;
      output_string flux "+"
    done  ;
    output_string flux "\n" ;
  in
  ecrire_separation () ;
  for i=0 to 3 do
    output_string flux "|" ;
    for j=0 to 3 do
      output_string flux table.(i).(j) ;
      output_string flux "|"
    done ;
    ecrire_separation ()
  done ;
  flush flux
;;
