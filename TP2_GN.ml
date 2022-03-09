(* GRAND NOMBRES *)

(* Le type général des grands nombres *)
type gdnb = {
    signe : bool;
    abs : (int*int) list
};;

(* Les différentes valeurs qui nous sert de tests *)
let x : gdnb = {
    signe  = false;
    abs = [
        (0, 9642);
        (2, 3465) (*-346500009642*)
    ]
};;

let y : gdnb = {
    signe = true;
    abs = [
        (0, 4334);
        (1, 7865);
        (2, 3456) (*345678654334*)
    ]
}

let a : gdnb = {
    signe  = true;
    abs = [
        (0, 9642);
        (2, 3725);
    ]
};;

let b : gdnb = {
    signe = true;
    abs = [
        (0, 4334);
        (1, 7865);
        (2, 3456);
        (3, 2412)
    ]
}

(* Autre fonction *)

    (* Recherche de la sous valeur *)
let rec search_value (x : (int * int) list) (i : int) : int = 
    match x with 
        | [] -> 0
        | (a,b)::c -> if a = i then b else search_value c i;;

    (* On change de signe souvent utilisé pour les nombre négatif *)
let change_sign (x : string) : string = String.sub x 1 ((String.length x) - 1)

(* Question 1 - Conversion entre STR et GDNB et inversement *)
    (* Conversion chaine de caractère vers GDNB *)
let gdnb_of_string (x : string) : gdnb =
    let is_positiv (x : string) : bool = int_of_string (String.sub x 0 4) > 0 in
    let get_number (x : string) : int = int_of_string (String.sub x (if String.length x >= 4 then String.length x - 4 else 0) (if String.length x >= 4 then 4 else String.length x)) in
    let reduce_string (x : string) : string = String.sub x 0 (String.length x -  (if String.length x >= 4 then 4 else String.length x)) in
        let rec get_list (x : string) (i : int) (l : (int * int) list) : (int * int) list =
            if String.length x = 0 then l
            else get_list (reduce_string x) (i + 1) (if get_number x <> 0 then l@[(i, get_number x)] else l) in
        {signe = is_positiv x;
         abs = get_list (if is_positiv x then x else (change_sign x)) 0 []};;

    (* Conversion GDNB vers une chaine de caractère *)
let string_of_gdnb (x : gdnb) : string =
    let complete_number (x : string) : string =
        if String.length x = 4 then x
        else if String.length x = 3 then "0" ^ x
        else if String.length x = 2 then "00" ^ x
        else if String.length x = 1 then "000" ^ x
        else "0000" ^ x in
        let rec get_str (x : (int * int) list) (p : int) (s : bool) (word : string) : string =
            match x with
                | [] -> if s then if String.length word > 0 then word else "0" else if String.length word > 0 then "-" ^ word else "0"
                | (i, v)::[] -> if i <> p then get_str ([(i,v)]@[]) (p + 1) s (complete_number "" ^ word)
                                else get_str [] (p + 1) s (string_of_int v ^ word)
                | (i, v)::q  -> if i <> p then get_str ([(i,v)]@q) (p + 1) s (complete_number "" ^ word)
                                else get_str q (p + 1) s (complete_number (string_of_int v) ^ word) in
            get_str x.abs 0 x.signe "";;

    (* test *)
let x_gdnb = gdnb_of_string "-346500009642";;
let p = gdnb_of_string "12340003876356280087635";;
let x_str = string_of_gdnb x;;

(* Question 2 - Comparaison entre deux variables de type GDNB *)
    (* Comparaison fainéante *)
let compare_gdnb (x : gdnb) (y : gdnb) : bool = 
    string_of_gdnb x > string_of_gdnb y;;

    (* Comparaison avec le type GDNB UNIQUEMENT *)
let comparison_gdnb (x : gdnb) (y : gdnb) : bool = 
    let rec comparison_gdnb_aux (x : (int * int) list) (y : (int * int) list) (r : bool): bool =
        match x with 
            | [] -> r 
            | (a,b)::c -> comparison_gdnb_aux c y (b >= (search_value y a)) in
    let rec max_position (x : (int * int) list) : int = 
        match x with 
            | [] -> -1
            | (a,b)::[] ->  a
            | (a,b)::c -> max_position c in
        if x.signe <> y.signe then x.signe > y.signe
        else if max_position x.abs <> max_position y.abs then (max_position x.abs) > (max_position y.abs)
        else if x.signe && y.signe then comparison_gdnb_aux x.abs y.abs false
        else comparison_gdnb_aux y.abs x.abs false;;

(* Question 3 - Somme de deux variable de type GDNB *)
    (* gérer la somme dans les deux sens y + x et x + y doivent être identiques *)
    (* gérer le - et le + *)
    (* valable que pour true true *)

    (* SOMME entre deux GDNB *)
let somme (m : gdnb) (n : gdnb) : gdnb = 
    let modified_value (x : int) : int = int_of_string (String.sub (string_of_int x) 1 4) in
    let get_retenue (x : int) : int = int_of_string (String.sub (string_of_int x) 0 1) in
    let rec maximum_position (x : (int*int) list) (i : int) : int = 
        match x with
            | [] -> i
            | (a,b)::c -> maximum_position c a in 
    let rec somme_gdnb_aux (x : (int*int) list) (y : (int*int) list) (r : (int*int) list) (index : int) (pos : int) (ret : int) : ((int*int) list) =
        if index > pos + 1 then r
        else somme_gdnb_aux 
                            x (* Premier parametre *) 
                            y (* Second parametre *) 
                            (* Troisième paramètre *)
                            (if (search_value x index) + ret + (search_value y index) = 0 then r 
                             else if (search_value x index) + ret + (search_value y index) > 9999
                                then r@[(index, modified_value ((search_value x index) + ret + (search_value y index)))] 
                             else r@[(index, (search_value x index) + ret + (search_value y index))]) 
                            (* Quatrième paramètre *)
                            (index + 1) 
                            (* Cinquième paramètre *)
                            pos 
                            (* Sixième et dernier paramètre *)
                            (if (search_value x index) + ret + (search_value y index) > 9999
                              then get_retenue ((search_value x index) + ret + (search_value y index))
                             else 0) in  
    let rec sub_gdnb_aux (x : (int*int) list) (y : (int*int) list) (r : (int*int) list) (index : int) (pos : int) (ret : int) : ((int*int) list) =
    if index > pos then r
    else sub_gdnb_aux 
                        x (* Premier parametre *) 
                        y (* Second parametre *) 
                        (* Troisième paramètre *)
                        (if (search_value x index) - ret - (search_value y index) = 0 then r 
                            else if (search_value x index) - ret - (search_value y index) < 0
                            then r@[(index, ((search_value x index) - ret - (search_value y index) + 10000))] 
                            else r@[(index, (search_value x index) - ret - (search_value y index))]) 
                        (* Quatrième paramètre *)
                        (index + 1) 
                        (* Cinquième paramètre *)
                        pos 
                        (* Sixième et dernier paramètre *)
                        (if (search_value x index) - ret - (search_value y index) < 0 then 1 else 0) in
        {signe = if (m.signe == n.signe) then m.signe else if comparison_gdnb m n then m.signe else n.signe;
        abs = if (m.signe == n.signe) then somme_gdnb_aux m.abs n.abs [] 0 (max (maximum_position m.abs 0) (maximum_position n.abs 0)) 0
                                      else if comparison_gdnb m n then sub_gdnb_aux m.abs n.abs [] 0 (max (maximum_position m.abs 0) (maximum_position n.abs 0)) 0
                                      else sub_gdnb_aux n.abs m.abs [] 0 (max (maximum_position m.abs 0) (maximum_position n.abs 0)) 0};;
        (* somme ++ -- <----> soustraction +-  *)(* {signe = (m.signe && n.signe) || comparison_gdnb m n; *)
        
(* Question 4 *)

let mult (x : gdnb) (y : gdnb) : gdnb = 
    {signe = true; abs = []};;

(* Question 5 *)
(* division euclidienne *)
(* exception Division_by_zero;;
let div (x : gdnb) (y : gdnb) : gdnb = 
    let rec maximum_position (x : (int*int) list) (i : int) : int = 
        match x with
            | [] -> i
            | (a,b)::c -> maximum_position c a in 
    if List.length y.abs = 0 then raise Division_by_zero 
    else if comparison_gdnb y x then {signe = x.signe && y.signe; abs = []}
    else if maximum_position x 0 = 0 then {signe = x.signe && y.signe; abs = [(0, )]} ;; *)

(* Question 6 *)