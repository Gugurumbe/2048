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
      let (i, j) = extraire (roulette mod !nombre_possibles) in
      jeu.(i).(j) <- Nombre(k) ;
    end ;
  !nombre_possibles <> 0
;;

let ecraser_vers jeu vect_i vect_j =
  let in_range k l = k >= 0 && k < 3 && l >= 0 && l < 3 in
  let rec pousser i j =
    match jeu.(i).(j) with
    | Libre -> ()
    | Nombre(k) when in_range (i + vect_i) (j + vect_j) ->
      begin
	match jeu.(i + vect_i).(j + vect_j) with
	| Libre ->
	  begin
	    jeu.(i+vect_i).(j+vect_j) <- jeu.(i).(j) ;
	    jeu.(i).(j) <- Libre ;
	    pousser (i+vect_j) (j+vect_j)
	  end
	| Nombre(l) when l = k ->
	  begin
	    jeu.(i+vect_i).(j+vect_j) <- Nombre(2*k) ;
	    jeu.(i).(j) <- Libre ;
	    pousser (i+vect_i) (j+vect_j)
	  end
	| _ -> ()
      end
    | _ -> ()
  in
  for i=0 to 3 do
    for j=0 to 3 do
      pousser ((-i * vect_i) mod 4) ((-j * vect_j) mod 4)
    done ;
  done 
;;
