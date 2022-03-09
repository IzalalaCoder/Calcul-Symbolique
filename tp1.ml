(*------------------------------------REPRESENTATION CREUSE PAR COEFF *)
type poly = (int*float) list;;

(* 2 - X + 5X³ *)
let polynome : poly =  [
	(0, 2.);
	(1, -1.); 
	(3, 5.)
];;

(* 6X + X^4 *)
let polyy : poly = [
	(1,6.);
	(4, 1.)
];;

(* 2XÂ² + 2X + 1 *)
let test : poly = [
	(2, 2.);
	(1, 2.);
	(0, 1.)
];;

(* 2X + 2X² *)
let exemple : poly = [
	(1, 2.);
	(2, 2.)
];;

(* TEST DE L'EXEMPLE POUR MULT NAIVE *)
(*degre, coeff*)
let a : poly = [
	(1, -1.);
	(2, 2.);
	(3, 1.)
];;

let b : poly = [
	(0, 2.);
	(1, -1.);
	(3, 3.);
	(5, -1.)
];;

(* SECOND TEST pour multnaive  *)  
let x : poly = [
	(0, 3.);
	(1, 1.);
	(2, 1.);
	(3, -2.)
];;

let y : poly = [
	(0, 1.);
	(1, -3.);
	(2, 5.)
];;

(* TEST pour le modulo *)
let mo : poly = [
	(0,2.);
	(1,-1.);
	(3,3.);
	(5,-1.)
];;

(* TEST POUR LINVERSION DE MERDE *)
let invvv : poly = [
	(0,1.);
	(1,2.);
	(2,1.);
	(3,1.)
];;

(* FONCTION COEFF -> val dg_to_coeff : int -> poly -> float = <fun> *)
let rec dg_to_coeff i (p : poly) = match p with 
	|  [] -> 0.0
	| (d,c)::q -> if d = i then c else dg_to_coeff i q;;
	
(* FONCTION SOMME DE DEUX POLYS 
	-> somme_poly : poly -> poly -> poly = <fun>  *)
let somme_poly (p1 : poly) (p2 : poly) =
	let rec new_poly p1 p2 (np : poly) = match p1 with
		| [] -> np
		| (d, c)::q -> if (dg_to_coeff d np) <> 0. then new_poly q p2 np 
					   else new_poly q p2 (np@[(d, c +. (dg_to_coeff d p2))]) 
		in new_poly p2 p1 (new_poly p1 p2 []);;

(* FONCTION MULTIPLICATION DE DEUX POLYS
	avec les 5 fonctions a dÃ©finir qui sont les suivantes :
		- multCoeff p a 
		- degre p 
		- multXn p n
		- cut p i
		- multnaive p q *)

(* RETOURNE LA MULTIPLICATION ENTRE UN POLY ET UN NOMBRE P * X *)
let multCoeff (p : poly) a = 
	let rec multCoeffAux p a (np : poly) = match p with
		| [] -> np
		| (d, c)::q -> multCoeffAux q a (np@[(d, c *. a)]) in 
			multCoeffAux p a [];;
			
(* RETOURNE LE DEGRE MAXIMAL DU POLY P *)
let degre (p : poly) = 
	let rec degreAux p max = match p with
		| [] -> max
		| (d,c)::q when d > max -> degreAux q d
		| (d,c)::q -> degreAux q max in
			degreAux p (fst (List.hd p));;

(* MULTIPLICATION P * XÂ² PAR EXEMPLE via la commande multXn p 2 *)
let multXn (p : poly) (n : int) = 
	let rec multAux p n (np : poly) = match p with
		| [] -> np
		| (d, c)::q -> multAux q n (np@[(n + d, c)]) in
		multAux p n [];;
		
(* cut polynome 2
		- P0 -> 2 - X
		- P1 -> 5X *)
let cut (p : poly) (i : int) = 
	let rec p1 p i (np : poly) = match p with 
		| [] -> np
		| (d, c)::q -> if d >= i then p1 q i np@[((d - i), c)]
						else p1 q i np in
		let rec p0 p i (np : poly) = match p with
			| [] -> np
			| (d, c)::q -> if d < i then p0  q i np@[(d, c)]
						   else p0 q i np in
			(p0 p i [], p1 p i []);;

(* FONCTION QUI RETOURNE LA MULTIPLICATION NAIVE *)
let multNaive (p : poly) (q : poly) =
	let rec multMonome n x (p : poly) (r : poly) = 
		match p with 
			| [] -> r
			| (d,c)::q -> multMonome n x q (r@[((d + n), (x *. c))]) in
	let rec multNaiveAux p q (r : poly) = 
		match p with 
			| [] -> r 
			| (d,c)::t -> multNaiveAux t q (somme_poly (multMonome d c q []) r) in
		multNaiveAux p q [];;	

(* Autre version KARATSUBA *)


(* FONCTION DIVISION DE DEUX POLYS
	avec les 3 fonctions a définir qui sont les suivantes :
		- renverse k p  
		- moduloXn p n 
		- INVERSE_MOD P M *)

(* Retourne le renversé d'ordre k de p *)
let renverse k (p : poly) = 
	let rec renverseAux k p (result : poly) =
		match p with
			| [] -> result
			| (d,c)::q -> renverseAux k q result@[(d - k, c)] in
		renverseAux k p [];;

(* Retourne le reste de la division de p par le monome X^n *)
let moduloXn (p : poly) (n : int) : poly = 
	let rec moduloXnAux p n (r : poly) = 
		match p with
			| [] -> r
			| (d,c)::q -> if d >= n then 
							if (d - n) > 0 then
								moduloXnAux q n (r@[(d - n, c)]) 
							else 
								moduloXnAux q n r
						  else 
							moduloXnAux q n r@[(d,c)] in
		moduloXnAux p n [];;

(* Retourne l'inverse de p modulo X^n méthode de NEWTON *)
(* let inverse_mod (p : poly) (m : int) : poly = 
	let rec inverse_mod_aux p m x t n : poly = 
		if m <= x then inverse_mod_aux p m x (2. *. x) (x ** 2.) (2. ** (x +. 1.))
		else somme_poly((moduloXn (multCoeff p (-1.)) n) (0, x)) in
		inverse_mod_aux p m 1. 0. 0.;;  *)
let inverse_mod (p : poly) (m : int) : poly = 
    let exposant x = int_of_float (2. ** ((float_of_int x) +. 1.)) in 
        let rec searchPi x bound polynome result =  
       		if x = bound then moduloXn result x
        	else searchPi (x + 1) bound polynome (somme_poly (multCoeff result 2.) ((multCoeff (multNaive (moduloXn polynome (exposant x)) (multNaive result result))) (-1.)))
         in searchPi 1 m p [(0, 1.)];;
		
(* Fonction qui Ã©value un polynome en une valeur selon le schÃ©ma de H *)
let horner p x =
	let rec hornerAux p x i e =
		if i < 0 then e
		else hornerAux p x (i - 1) (e *. x +. (dg_to_coeff i p)) in
		hornerAux p x ((degre p) - 1) (dg_to_coeff (degre p) p);;

(*------------------------------------------INTERPOLATION DE LAGRANGE *)

(* RETOURNE LE POLYNOME Li *)
(* let li (p : poly) (j : int) : poly = 
	let rec liAux p j n = 
		if n <> j then somme_poly p [()] *)


(* CALCULE LE POLY Pf *)
(* let P   *)