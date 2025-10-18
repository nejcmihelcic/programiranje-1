(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Collatzovo zaporedje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Collatzovo zaporedje se začne s pozitivnim naravnim številom $a_0$ ter
 nadaljuje kot:

 $$a_{n + 1} = \begin{cases} a_n / 2, & \text{če je } a_n \text{ sodo} \\ 3 a_n
 + 1, & \text{če je } a_n \text{ liho} \end{cases}$$

 Sestavite funkcijo `collatz : int -> int list`, ki sprejme začetni člen
 zaporedja in vrne seznam vseh členov, dokler zaporedje ne doseže $1$.
[*----------------------------------------------------------------------------*)

let rec collatz n =
  match n with
  | 1 -> [1]
  | n when n mod 2 = 0 -> n :: collatz (n / 2)
  | n -> n :: collatz (3 * n + 1)

let primer_ogrevanje_1 = collatz 1024
(* val primer_ogrevanje_1 : int list =
  [1024; 512; 256; 128; 64; 32; 16; 8; 4; 2; 1] *)

let primer_ogrevanje_2 = collatz 27
(* val primer_ogrevanje_2 : int list =
  [27; 82; 41; 124; 62; 31; 94; 47; 142; 71; 214; 107; 322; 161; 484; 242;
   121; 364; 182; 91; 274; 137; 412; 206; 103; 310; 155; 466; 233; 700; 350;
   175; 526; 263; 790; 395; 1186; 593; 1780; 890; 445; 1336; 668; 334; 167;
   502; 251; 754; 377; 1132; 566; 283; 850; 425; 1276; 638; 319; 958; 479;
   1438; 719; 2158; 1079; 3238; 1619; 4858; 2429; 7288; 3644; 1822; 911;
   2734; 1367; 4102; 2051; 6154; 3077; 9232; 4616; 2308; 1154; 577; 1732;
   866; 433; 1300; 650; 325; 976; 488; 244; 122; 61; 184; 92; 46; 23; 70; 35;
   106; 53; 160; 80; 40; 20; 10; 5; 16; 8; 4; 2; 1] *)

(*----------------------------------------------------------------------------*
 ### Fiksne točke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `fiksne_tocke : ('a -> 'a) -> 'a list -> 'a list`, ki za
 dano funkcijo in seznam vrne podseznam vseh elementov, ki so fiksne točke.
[*----------------------------------------------------------------------------*)
let fiksne_tocke f lst = 
  List.filter (fun x -> f(x)=x) lst

let primer_ogrevanje_3 = fiksne_tocke (fun x -> x * x) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_3 : int list = [0; 1] *)

let primer_ogrevanje_4 = fiksne_tocke List.rev [[3]; [1; 4; 1]; [5; 9; 2; 6]; [5; 3; 5]; []; [8; 9; 7; 9; 3; 2; 3]]
(* val primer_ogrevanje_4 : int list list = [[3]; [1; 4; 1]; [5; 3; 5]; []] *)

(*----------------------------------------------------------------------------*
 ### Združevanje z ločilom
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sep_concat : 'a -> 'a list list -> 'a list`, ki združi
 seznam seznamov, pri čemer med elemente različnih seznamov ter na začetek in
 konec vstavi dano ločilo.
[*----------------------------------------------------------------------------*)

let sep_concat n lst =
  let operator sep acc el =
    acc @ el @ [sep] in
  List.fold_left (operator n) [n] lst

let primer_ogrevanje_5 = sep_concat 42 [[1; 2; 3]; [4; 5]; []; [6]]
(* val primer_ogrevanje_5 : int list = [42; 1; 2; 3; 42; 4; 5; 42; 42; 6; 42] *)

let primer_ogrevanje_6 = sep_concat 42 []
(* val primer_ogrevanje_6 : int list = [42] *)

(*----------------------------------------------------------------------------*
 ### Razbitje seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `partition : int -> 'a list -> 'a list`, ki sprejme pozitivno
 naravno število $k$ in seznam $[a_0, \dots, a_n]$ ter ga razdeli na zaporedne
 podsezname $[a_0, \dots, a_{k - 1}], [a_k, \dots, a_{2 k - 1}], \dots$, pri
 čemer je zadnji podseznam lahko tudi krajši.
[*----------------------------------------------------------------------------*)

let rec partition n lst =
  let rec aux current acc lst =
    match lst with
    | [] ->
      if current = [] then List.rev acc
      else List.rev (List.rev current :: acc) 
    | x::xs ->
      if List.length current < n then 
        aux (x :: current) acc xs
      else 
        aux [x] (List.rev current :: acc) xs
  in 
  aux [] [] lst

let primer_ogrevanje_7 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_ogrevanje_7 : int list list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] *)

let primer_ogrevanje_8 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_8 : int list list =
  [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)

let phi1 (a, b) = (b, a)
let psi1 (b, a) = (a, b)

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let phi2 a =
  match a with
  | In1 x -> In2 x
  | In2 y -> In1 y
let psi2 a = 
  match a with
  | In2 x -> In1 x
  | In1 y -> In2 y

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3 (a, (b,c)) = ((a,b),c)
let psi3 ((a,b),c) = (a,(b,c))

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4 a = 
  match a with
  | In1 x -> In1 (In1 x)
  | In2 (In1 y) -> In1 (In2 y)
  | In2 (In2 z) -> In2 z

let psi4 a = 
  match a with
  | In1 (In1 x) -> In1 x
  | In1 (In2 y) -> In2 (In1 y)
  | In2 z -> In2 (In2 z)

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 (a,b) = 
  match b with
  | In1 x -> In1 (a,x)
  | In2 y -> In2 (a,y)
let psi5 a = 
  match a with
  | In1 (x,y) -> (x, In1 y)
  | In2 (x,y) -> (x, In2 y)

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6 f = (fun b -> f (In1 b), fun a -> f (In2 a))
let psi6 (f,g) = function
    | In1 b -> f b
    | In2 c -> g c

(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 f = ((fun x-> fst (f x)), (fun x-> snd (f x)))
let psi7 (f,g) = fun x -> (f x, g x)

(*----------------------------------------------------------------------------*
 ## Permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Permutacije so preureditve elementov $\{0, 1, \dots, n-1\}$, torej bijektivne
 preslikave $$p \colon \{0, 1, \dots, n-1\} \to \{0, 1, \dots, n-1\}.$$ V nalogi
 bomo permutacije predstavili s seznamom števil, v katerem je na $i$-tem mestu
 seznama zapisana slika elementa $i$.
 Na primer, permutaciji $0 \, 1 \, 2 \, 3 \, 4 \, 5 \choose 5 \, 3 \, 2 \, 1 \,
 4 \, 0$ in $0 \, 1 \, 2 \, 3 \, 4 \, 5 \, 6 \, 7 \, 8 \, 9 \choose 3 \, 9 \, 1
 \, 7 \, 5 \, 4 \, 6 \, 8 \, 2 \, 0$ bi zapisali s seznamoma:
[*----------------------------------------------------------------------------*)

let permutacija_1 = [5; 3; 2; 1; 4; 0]
let permutacija_2 = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0]
(* val permutacija_1 : int list = [5; 3; 2; 1; 4; 0] *)
(* val permutacija_2 : int list = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0] *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kompozitum : int list -> int list -> int list`, ki sprejme
 dve permutaciji in vrne njun kompozitum. Za permutaciji $p$ in $q$, je njun
 kompozitum funkcija

 $$ p \circ q \colon i \mapsto p ( q ( i ) ). $$

 Predpostavite lahko, da sta seznama enakih dolžin.
[*----------------------------------------------------------------------------*)

let kompozitum p q = 
  let rec aux p e =
    match p with
    |x::xs when e = 0 -> x
    |x::xs -> aux xs (e-1)
    |[] -> 0
  in 
  List.map (aux p) q


let primer_permutacije_1 = kompozitum permutacija_1 permutacija_1
(* val primer_permutacije_1 : int list = [0; 1; 2; 3; 4; 5] *)

let primer_permutacije_2 = kompozitum permutacija_2 permutacija_2
(* val primer_permutacije_2 : int list = [7; 0; 9; 8; 4; 5; 6; 2; 1; 3] *)

(*----------------------------------------------------------------------------*
 ### Inverz
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napiši funkcijo `inverz : int list -> int list`, ki vrne inverz dane
 permutacije $p$, torej tako permutacijo $p^{-1}$, da velja $$p \circ p^{-1} =
 \mathrm{id},$$ kjer je $\mathrm{id}$ indentiteta.
[*----------------------------------------------------------------------------*)

let inverz p = 
  let rec aux p c i =
    match p with
    | x::xs when x=i -> c
    | x::xs -> aux xs (c+1) i
    | [] -> 0
  in
  List.init (List.length p) (aux p 0)

let primer_permutacije_3 = inverz permutacija_1
(* val primer_permutacije_3 : int list = [5; 3; 2; 1; 4; 0] *)

let primer_permutacije_4 = inverz permutacija_2
(* val primer_permutacije_4 : int list = [9; 2; 8; 0; 5; 4; 6; 3; 7; 1] *)

let primer_permutacije_5 = kompozitum permutacija_2 (inverz permutacija_2)
(* val primer_permutacije_5 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(*----------------------------------------------------------------------------*
 ### Razcep na cikle
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `cikli : int list -> int list list`, ki za dano permutacijo
 vrne seznam ciklov, ki to permutacijo sestavljajo. Vsak element $\{0, 1, \dots,
 n-1\}$ naj se pojavi v natanko enem ciklu.
[*----------------------------------------------------------------------------*)

let cikli p = 
  let n = List.length(p) in 
  let visited = Array.make n false in
  let get_elem p i = 
    let rec aux acc = function
    | x::xs when acc = i -> x
    | _::xs -> aux (acc+1) xs
    | [] -> -1
    in
    aux 0 p
  in
  let cikel p e =
    let rec aux = function
    |cur when cur=(-1) -> e::aux (get_elem p e)
    |cur when cur=e -> []
    |cur -> cur::aux (get_elem p cur)
    in
    aux (-1)
  in
  let naredi_cikel p visited acc el =
    if not visited.(el) then
      (let c = cikel p el in
      List.iter (fun x -> (visited.(x)<-true)) c;
      c::acc
      ) else acc
  in
  let rec loop i acc =
    if i = n then List.rev acc
    else
      let acc' = naredi_cikel p visited acc i in 
      loop (i+1) acc'
    in
    loop 0 []
    
let primer_permutacije_6 = cikli permutacija_1
(* val primer_permutacije_6 : int list list = [[0; 5]; [1; 3]; [2]; [4]] *)

let primer_permutacije_7 = cikli permutacija_2
(* val primer_permutacije_7 : int list list =
  [[0; 3; 7; 8; 2; 1; 9]; [4; 5]; [6]] *)

let primer_permutacije_8 = cikli (inverz permutacija_2)
(* val primer_permutacije_8 : int list list =
  [[0; 9; 1; 2; 8; 7; 3]; [4; 5]; [6]] *)

(*----------------------------------------------------------------------------*
 ### Transpozicije permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Vsako permutacijo lahko zapišemo kot produkt transpozicij, torej menjav dveh
 elementov. Na primer, permutacijo $0 \, 1 \, 2 \, 3 \choose 1 \, 0 \, 3 \, 2$
 dobimo kot produkt transpozicij $(0, 1) \circ (2, 3)$.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `iz_transpozicij : int -> (int * int) list -> int list`, ki
 sprejme dolžino permutacije in seznam transpozicij ter vrne permutacijo, ki jim
 ustreza.
[*----------------------------------------------------------------------------*)

let iz_transpozicij n perm =
  let rec find_el perm i =
    match perm with
    | [] -> i
    | x::xs when (fst x = i) -> find_el xs (snd x)
    | x::xs when (snd x = i) -> find_el xs (fst x)
    | x::xs -> find_el xs i
  in
  let rec loop acc i =
    match i with
    | i when i=n -> List.rev acc
    | i -> loop ((find_el (List.rev perm) i)::acc) (i+1)
  in
  loop [] 0

let primer_permutacije_9 = iz_transpozicij 4 [(0, 1); (2, 3)]
(* val primer_permutacije_9 : int list = [1; 0; 3; 2] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `v_transpozicije : int list -> (int * int) list`, ki zapiše
 permutacijo kot produkt transpozicij, torej menjav dveh elementov. Možnih
 produktov je več, veljati mora le, da je kompozicija dobljenih ciklov enaka
 prvotni permutaciji.

 *Namig: Pomagate si lahko z lastnostjo, da poljubni cikel razpade na
 transpozicije po naslednji formuli*
 $$(i_1, i_2, i_3, \ldots, i_{k-1}, i_k) = (i_1, i_k)\circ(i_1,
 i_{k-1})\circ(i_1, i_3)\circ(i_1, i_2).$$
[*----------------------------------------------------------------------------*)

let v_transpozicije p = 
  let cik = cikli p in
  let cik_to_tr c =
    let ar = Array.of_list c in
    let i1 = ar.(0) in
    let ik = ar.(Array.length ar - 1) in
    let rec loop acc i =
      match i with
      | i when ar.(i)=ik -> (i1,ik)::acc 
      | i -> loop ((i1,ar.(i))::acc) (i+1)
    in
    loop [] 1
  in
  let rec aux acc p = 
   match p with
   | x :: xs when List.length x = 1 -> aux acc xs
   | x :: xs -> aux (cik_to_tr x :: acc) xs
   | [] -> acc 
  in List.flatten (List.rev (aux [] cik))

let primer_permutacije_10 = v_transpozicije permutacija_1
(* val primer_permutacije_10 : (int * int) list = [(0, 5); (1, 3)] *)

let primer_permutacije_11 = v_transpozicije permutacija_2
(* val primer_permutacije_11 : (int * int) list =
  [(0, 9); (0, 1); (0, 2); (0, 8); (0, 7); (0, 3); (4, 5)] *)

(*----------------------------------------------------------------------------*
 ## Sudoku
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sudoku je igra, v kateri mrežo $9 \times 9$ dopolnimo s števili od $1$ do $9$,
 tako da se nobeno število v nobeni vrstici, stolpcu ali eni od devetih škatel
 velikosti $3 \times 3$ ne ponovi. Primer začetne postavitve in ustrezne rešitve
 je:

 ```plaintext
 +-------+-------+-------+       +-------+-------+-------+
 | 5 4 . | . 7 . | . . . |       | 5 4 3 | 6 7 8 | 9 1 2 |
 | 6 . . | 1 9 5 | . . . |       | 6 7 2 | 1 9 5 | 3 4 8 |
 | . 9 8 | . . . | . 6 . |       | 1 9 8 | 3 4 2 | 5 6 7 |
 +-------+-------+-------+       +-------+-------+-------+
 | 8 . . | . 6 . | . . 3 |       | 8 1 9 | 7 6 4 | 2 5 3 |
 | 4 . . | 8 . 3 | . . 1 |       | 4 2 6 | 8 5 3 | 7 9 1 |
 | 7 . . | . 2 . | . . 6 |       | 7 3 5 | 9 2 1 | 4 8 6 |
 +-------+-------+-------+       +-------+-------+-------+
 | . 6 . | . . 7 | 8 . . |       | 9 6 1 | 5 3 7 | 8 2 4 |
 | . . . | 4 1 9 | . . 5 |       | 2 8 7 | 4 1 9 | 6 3 5 |
 | . . . | . 8 . | . 7 9 |       | 3 5 4 | 2 8 6 | 1 7 9 |
 +-------+-------+-------+       +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Delno izpolnjen sudoku bomo predstavili s tabelo tabel tipa `int option array
 array`, kjer bomo prazna mesta označili z `None`, rešen sudoku pa s tabelo
 tabel običajnih števil.
[*----------------------------------------------------------------------------*)

type mreza = int option array array
type resitev = int array array

(*----------------------------------------------------------------------------*
 Na primer, zgornjo mrežo in rešitev bi predstavili s seznamoma:
[*----------------------------------------------------------------------------*)

let primer_mreze : mreza = [|
 [|None; None; None; Some 9; None; Some 2; Some 7; Some 3; Some 8|];
 [|None; Some 6; None; None; None; Some 5; None; None; Some 9|];
 [|Some 8; Some 2; Some 9; Some 3; None; None; Some 6; None; Some 1|];
 [|None; None; Some 1; None; None; Some 6; None; Some 8; Some 2|];
 [|None; Some 9; Some 3; None; Some 1; None; Some 4; None; Some 5|];
 [|None; Some 8; None; Some 7; None; None; Some 9; Some 1; None|];
 [|Some 9; None; None; None; None; Some 7; None; None; None|];
 [|None; Some 3; Some 8; Some 6; None; None; Some 5; None; None|];
 [|None; None; None; Some 5; None; Some 1; Some 8; Some 9; Some 4|]
|]

let primer_resitve : resitev = [|
  [|5; 4; 3; 6; 7; 8; 9; 1; 2|];
  [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
  [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
  [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
  [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
  [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
  [|9; 6; 1; 5; 3; 7; 8; 2; 4|];
  [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
  [|3; 5; 4; 2; 8; 6; 1; 7; 9|];
|]
(* val primer_mreze : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; None|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)
(* val primer_resitve : resitev =
  [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
    [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
    [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
    [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
    [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Dopolnitev mreže
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dodaj : int -> int -> int -> mreza -> mreza` tako da `dodaj
 i j n m` vrne mrežo, ki je povsod enaka mreži `m`, le na mestu v vrstici `i` in
 stolpcu `j` ima zapisano število `n`.

 **Pozor:** OCaml dopušča spreminjanje tabel (o tem se bomo učili kasneje). Vaša
 funkcija naj te možnosti ne uporablja, temveč naj sestavi in vrne novo tabelo.
[*----------------------------------------------------------------------------*)

let dodaj i j n mreza = 
  let vrstica = 
    Array.mapi (fun ci e -> if (ci=j) then (Some n) else e) mreza.(i)
  in
  Array.mapi (fun ri row -> if ri=i then vrstica else row) mreza

let primer_sudoku_1 = primer_mreze |> dodaj 0 8 2
(* val primer_sudoku_1 : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; Some 2|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Izpiši mrežo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkciji `izpis_mreze : mreza -> string` in `izpis_resitve : resitev
 -> string`, ki sprejmeta mrežo oziroma rešitev in vrneta niz, ki predstavlja
 izpis v zgornji obliki.
[*----------------------------------------------------------------------------*)

let izpis_mreze (mreza : mreza) =
  let sep = "+-------+-------+-------+\n" in 
  let row = "| . . . | . . . | . . . |\n" in
  let spremeni_vrstico (v : int option array) =
    snd @@ String.fold_left (
      fun (k, acc) c ->
        if c='.' then
          let new_char =
            match v.(k) with
            | Some n -> Char.chr (n + Char.code '0')
            | None -> '.'
          in
          (k+1, acc ^ String.make 1 new_char)
        else
          (k, acc ^ String.make 1 c))
      (0, "") 
      row
  in
  let rec loop (i : int) (acc : string) : string =
    if (i+1) = 9 then
      acc ^ spremeni_vrstico mreza.(i) ^ sep
    else if ((i+1) mod 3) = 0 then
      loop (i+1) (acc ^ spremeni_vrstico mreza.(i) ^ sep)
    else 
      loop (i+1) (acc ^ spremeni_vrstico mreza.(i))
  in
  loop 0 sep

      

let primer_sudoku_2 = primer_mreze |> izpis_mreze |> print_endline
(* 
  +-------+-------+-------+
  | 5 4 . | . 7 . | . . . |
  | 6 . . | 1 9 5 | . . . |
  | . 9 8 | . . . | . 6 . |
  +-------+-------+-------+
  | 8 . . | . 6 . | . . 3 |
  | 4 . . | 8 . 3 | . . 1 |
  | 7 . . | . 2 . | . . 6 |
  +-------+-------+-------+
  | . 6 . | . . 7 | 8 . . |
  | . . . | 4 1 9 | . . 5 |
  | . . . | . 8 . | . 7 9 |
  +-------+-------+-------+
  
  val primer_sudoku_2 : unit = ()
*)

let izpis_resitve (resitev : resitev) = 
  let sep = "+-------+-------+-------+\n" in 
  let row = "| . . . | . . . | . . . |\n" in
  let spremeni_vrstico (v : int array) : string =
    snd @@ String.fold_left (
      fun (k, acc) c ->
        if c='.' then
          let new_char = Char.chr (v.(k) + Char.code '0') in 
          ((k+1), acc ^ String.make 1 new_char)
        else
          (k, acc ^ String.make 1 c))
      (0, "")
      row
  in 
  let rec loop acc k =
    if k=8 then
      acc ^ spremeni_vrstico resitev.(k) ^ sep
    else if k=2 || k=5 then
      loop (acc ^ spremeni_vrstico resitev.(k) ^ sep) (k+1)
    else
      loop (acc ^ spremeni_vrstico resitev.(k)) (k+1)
  in
  loop sep 0  

let primer_sudoku_3 = primer_resitve |> izpis_resitve |> print_endline
(*
  +-------+-------+-------+
  | 5 4 3 | 6 7 8 | 9 1 2 |
  | 6 7 2 | 1 9 5 | 3 4 8 |
  | 1 9 8 | 3 4 2 | 5 6 7 |
  +-------+-------+-------+
  | 8 1 9 | 7 6 4 | 2 5 3 |
  | 4 2 6 | 8 5 3 | 7 9 1 |
  | 7 3 5 | 9 2 1 | 4 8 6 |
  +-------+-------+-------+
  | 9 6 1 | 5 3 7 | 8 2 4 |
  | 2 8 7 | 4 1 9 | 6 3 5 |
  | 3 5 4 | 2 8 6 | 1 7 9 |
  +-------+-------+-------+

  val primer_sudoku_3 : unit = ()
*)

(*----------------------------------------------------------------------------*
 ### Preveri, ali rešitev ustreza mreži
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ustreza : mreza -> resitev -> bool`, ki preveri, ali rešitev
 ustreza dani mreži. Rešitev ustreza mreži, če se na vseh mestih, kjer je v
 mreži podana številka, v rešitvi nahaja enaka številka.
[*----------------------------------------------------------------------------*)

let ustreza (mreza : mreza) (resitev : resitev) : bool = 
  let rec preveri_vrstico vrstica el =
    if el = 9 then true
    else
      match (mreza.(vrstica)).(el) with
      | Some n when (resitev.(vrstica)).(el)=n ->
        preveri_vrstico vrstica (el+1)
      | Some n -> false
      | None -> preveri_vrstico vrstica (el+1)
  in
  let rec loop i =
    if i = 9 then true
    else if preveri_vrstico i 0 = true then
      loop (i+1)
    else
      false
  in
  loop 0

let primer_sudoku_4 = ustreza primer_mreze primer_resitve
(* val primer_sudoku_4 : bool = true *)

(*----------------------------------------------------------------------------*
 ### Kandidati za dano prazno mesto
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcije `ni_v_vrstici`, `ni_v_stolpcu` in `ni_v_skatli`, vse tipa
 `mreza * int -> int -> bool`, ki preverijo, ali se v določeni vrstici, stolpcu
 oziroma škatli mreže ne nahaja dano število. Vrstice, stolpci in škatle so
 indeksirani kot:

 ```plaintext
     0 1 2   3 4 5   6 7 8
   +-------+-------+-------+
 0 |       |       |       |
 1 |   0   |   1   |   2   |
 2 |       |       |       |
   +-------+-------+-------+
 3 |       |       |       |
 4 |   3   |   4   |   5   |
 5 |       |       |       |
   +-------+-------+-------+
 6 |       |       |       |
 7 |   6   |   7   |   8   |
 8 |       |       |       |
   +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

let ni_v_vrstici ((m,v) : mreza * int) (n : int) : bool  = 
  let row = (m).(v) in 
  let rec aux i =
    if i=9 then true
    else if row.(i) = (Some n) then false
    else aux (i+1)
  in aux 0

let primer_sudoku_5 = ni_v_vrstici (primer_mreze, 0) 1
(* val primer_sudoku_5 : bool = true *)

let primer_sudoku_6 = ni_v_vrstici (primer_mreze, 1) 1
(* val primer_sudoku_6 : bool = false *)

let ni_v_stolpcu (m,s) n  = 
  let rec aux i =
    if i=9 then true
    else if (m.(i)).(s) = (Some n) then false
    else aux (i+1)
  in aux 0

let ni_v_skatli (m,s) n  =
  let row =
    if s < 3 then 0
    else if s>5 then 6
    else 3 in
  let column =
    if (s mod 3) = 0 then 0
    else if (s mod 3) = 1 then 3
    else 6 in
  let preveri_vrstico r c =
    let v=m.(r) in
    if v.(c) <> (Some n) && v.(c+1) <> (Some n) && v.(c+2) <> (Some n) then true
    else false
  in
  if preveri_vrstico row column && preveri_vrstico (row+1) column && preveri_vrstico (row+2) column then true
  else false
(*----------------------------------------------------------------------------*
 Napišite funkcijo `kandidati : mreza -> int -> int -> int list option`, ki
 sprejme mrežo in indeksa vrstice in stolpca praznega mesta ter vrne seznam vseh
 številk, ki se lahko pojavijo na tem mestu. Če je polje že izpolnjeno, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)

let get1 (mreza : mreza) (row : int) (column : int) : int option =
  (mreza.(row)).(column)

let kandidati (mreza : mreza) (r : int) (c : int) : int list option = 
  if Option.is_some @@ get1 mreza r c then None
  else
    let to_box r c =
      let s1 = c / 3 in 
      if r<3 then s1
      else if r>5 then s1+6
      else s1+3
    in
    let preveri r c n = 
      let b = to_box r c in
      ni_v_vrstici (mreza,r) n && ni_v_stolpcu (mreza, c) n && ni_v_skatli (mreza, b) n
    in
    let rec aux acc i : int list option =
      if not (get1 mreza r c = None) || (i=10 && acc=[]) then None
      else if i = 10 then Some (List.rev acc)
      else if preveri r c i then aux (i::acc) (i+1)
      else aux acc (i+1)
    in aux [] 1
      

let primer_sudoku_7 = kandidati primer_mreze 0 2
(* val primer_sudoku_7 : int list option = Some [1; 2; 3] *)

let primer_sudoku_8 = kandidati primer_mreze 0 0
(* val primer_sudoku_8 : int list option = None *)

(*----------------------------------------------------------------------------*
 ### Iskanje rešitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `resi : mreza -> resitev option`, ki izpolni mrežo sudokuja.
 Če je dana mreža rešljiva, mora funkcija najti rešitev, ki ustreza začetni
 mreži in jo vrniti v obliki `Some resitev`, sicer naj vrne `None`.
 Predpostavite lahko, da je rešitev enolična, zato lahko funkcija vrne prvo, ki
 jo najde.

 *Namig: Poiščite celico mreže z najmanj kandidati in rekurzivno preizkusite vse
 možnosti.*
[*----------------------------------------------------------------------------*)

let rec resi (mreza : mreza) : resitev option =
  let nxt i j =
    if j<8 then (i,j+1)
    else if j=8 && i<8 then (i+1,0)
    else (10,10) in
  let rec poisci_kandidata i j (acc : int * int) n : ((int * int) * int) option =
    let k = kandidati mreza i j in
    let i1 = fst @@ nxt i j in 
    let j1 = snd @@ nxt i j in
    match k with
    | None when get1 mreza i j = None -> None
    | None when j=8 && i=8 -> if n=10 then None else Some (acc, n)
    | None -> poisci_kandidata i1 j1 acc n 
    | Some lst when List.length lst = 1 -> Some ((i,j), 1)
    | Some lst when List.length lst < n ->
      if i=8 && j=8 then Some ((i,j), List.length lst)
      else poisci_kandidata i1 j1 (i,j) (List.length lst)
    | Some lst -> if (i=8 && j=8) then Some (acc, n) else poisci_kandidata i1 j1 acc n
  in 
  let je_resena mr : bool =
    let rec aux i j : bool =
      if i=10 then true
      else
        let i1=fst @@ nxt i j in
        let j1=snd @@ nxt i j in
        match get1 mr i j with
        | None -> false
        | Some n -> aux i1 j1
      in
    aux 0 0 
  in 
  let pretvori (mr : mreza) : resitev =
    let conv_row (r : int option array) : int array =
      Array.of_list @@ List.rev @@ Array.fold_left (
        fun acc a ->
          match a with
          | None -> -1::acc
          | Some n -> n::acc
      ) [] r 
    in
    Array.of_list @@ List.rev @@ Array.fold_left (
      fun acc a ->
        (conv_row a) :: acc
    ) [] mr
  in
  let get_candidate mr i j =
    let k = kandidati mr i j in 
    match k with
    | None -> -1
    | Some lst -> List.hd lst
  in
  let try_candidate (mr : mreza) (i : int) (j : int) (k : int list) : resitev option =
    List.find_map (fun n -> resi @@ dodaj i j n mr) k
  in
  match poisci_kandidata 0 0 (0,0) 10 with
  | None ->
    if (je_resena mreza = true) then (Some (pretvori mreza))
    else None
  | Some ((i,j), n) ->
    if n=1 then
      let k = get_candidate mreza i j in
      resi @@ dodaj i j k mreza
    else
      let k = kandidati mreza i j in
      match k with
      | None -> None
      | Some lst -> try_candidate mreza i j lst

let primer_sudoku_9 = resi primer_mreze
(* val primer_sudoku_9 : resitev option =
  Some
   [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
     [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
     [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
     [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
     [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)