(*----------------------------------------------------------------------------*
 # 2. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 V domači nalogi bomo preučevali _končne avtomate_, enostavne matematične modele
 računanja. Končni avtomati sicer ne morejo opisati vseh možnih izračunov, so pa
 zelo uporabni za prepoznavanje vzorcev v nizih.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Deterministični končni avtomati
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 _Deterministični končni avtomat_ (_deterministic finite automaton_ oz. DFA) nad
 _abecedo_ $\Sigma$ je sestavljen iz _množice stanj_ $Q$ ter _prehodne funkcije_
 $\delta : Q \times \Sigma \rightharpoonup Q$ med njimi. Avtomat začne v enem
 izmed možnih stanj $q_0$, nato pa glede na trenutno stanje in trenutni simbol
 preide v neko novo stanje in od tam nadaljuje z naslednjim znakom. Če ob
 pregledu celotnega niza konča v enem od _sprejemnih stanj_ $F \subseteq Q$, je
 niz sprejet, sicer pa ni. Prehodna funkcija $\delta$ je delno definirana. Če za
 trenutno stanje in simbol prehod ne obstaja, avtomat niz zavrne.

 Za primer si oglejmo avtomat, ki sprejema nize, sestavljene iz ničel in enic, v
 katerih je število enic deljivo s tri. Tak avtomat predstavimo z naslednjim
 diagramom, na katerem je začetno stanje označeno s puščico, sprejemna stanja pa
 so dvojno obkrožena.

 ![DFA](slike/dfa.png)

 V tem avtomatu je abeceda $\Sigma = \{ 0, 1\}$, potrebujemo pa tri stanja, za
 vsak ostanek enega, zato je $Q = \{ q_0, q_1, q_2 \}$. Začetno stanje je $q_0$,
 ki je hkrati tudi edino sprejemno stanje. Prehodna funkcija je definirana kot:

 | $\delta$ | $0$   | $1$   |
 | -------- | ----- | ----- |
 | $q_0$    | $q_0$ | $q_1$ |
 | $q_1$    | $q_1$ | $q_2$ |
 | $q_2$    | $q_2$ | $q_0$ |

 Če avtomat na primer prejme niz $10011$, bo prehajal skozi stanja:
 - Začetno stanje: $q_0$
 - Prebere $1$: prehod v $q_1$
 - Prebere $0$: stanje ostane $q_1$
 - Prebere $0$: stanje ostane $q_1$
 - Prebere $1$: prehod v $q_2$
 - Prebere $1$: prehod v $q_0$

 Ker je stanje $q_0$ sprejemno, avtomat niz sprejme.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Modul `DFA`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri implementaciji se bomo omejili na avtomate, ki delujejo nad znaki angleške
 abecede, stanja pa bomo zaradi preglednosti predstavili kar z nizi. Take
 avtomate predstavimo s signaturo `DFA_SIG`:
[*----------------------------------------------------------------------------*)

module type DFA_SIG = sig
  type stanje = string
  type t

  (* Funkcije za grajenje *)
  (* Argument tipa [bool] pove, ali naj bo dodano stanje sprejemno *)
  val ustvari : stanje -> bool -> t
  val dodaj_stanje : stanje -> bool -> t -> t
  val dodaj_prehod : stanje -> char -> stanje -> t -> t

  (* Funkcije za poizvedovanje *)
  val seznam_stanj : t -> stanje list
  val zacetno_stanje : t -> stanje
  val je_sprejemno_stanje : t -> stanje -> bool
  val prehodna_funkcija : t -> stanje -> char -> stanje option
  val seznam_prehodov : t -> (stanje * char * stanje) list
end

(*----------------------------------------------------------------------------*
 Napišite modul `DFA`, ki zadošča zgornji signaturi.
[*----------------------------------------------------------------------------*)

module DFA : DFA_SIG = struct
  type stanje = string
  type stanja = (string * bool) list
  type prehodi = (string * char * string) list

  type t = (stanja * prehodi)

  let ustvari (p : stanje) (v : bool) : t = ([(p, v)], [])
  let dodaj_stanje (p : stanje) (v : bool) (dfa : t) : t = 
    let nova_stanja : stanja = (p, v) :: fst dfa in
    let prehod : prehodi = snd dfa in 
    (nova_stanja, prehod)
  let dodaj_prehod (zac : stanje) (c : char) (konc : stanje) (dfa : t) : t = 
    let nova_stanja = fst dfa in
    let novi_prehod = (zac, c, konc) in
    let novi_prehodi = novi_prehod :: snd dfa in
    (nova_stanja, novi_prehodi)

  let seznam_stanj (dfa : t) : string list =
    let rec aux acc (st : stanja) =
      match st with
      | [] -> acc
      | x::xs -> aux ((fst x) :: acc) xs
    in aux [] (fst dfa)
  let zacetno_stanje (dfa : t) : stanje = 
    let rec aux cur st = 
      match st with
      | [] -> cur 
      | x::xs -> aux (fst x) xs 
    in aux "" (fst dfa)
  let je_sprejemno_stanje (dfa : t) (s : stanje) =
    let rec aux st =
      match st with
      | [] -> false
      | x::xs when (fst x) = s -> (snd x)
      | x::xs -> aux xs
    in aux (fst dfa)      
  let prehodna_funkcija (dfa : t) (st : stanje) (ch : char) : stanje option =
    let preh = snd dfa in 
    let rec aux (prehod : prehodi) =
      match prehod with
      | [] -> None
      | (z, c, k)::xs when z=st && c=ch -> Some k
      | x::xs -> aux xs
    in aux preh 
  let seznam_prehodov (dfa : t)  = snd dfa
end 

(*----------------------------------------------------------------------------*
 Primer zgornjega avtomata bi lahko zapisali kot:
[*----------------------------------------------------------------------------*)

let enke_deljive_s_3 = DFA.(
    ustvari "q0" true
    |> dodaj_stanje "q1" false
    |> dodaj_stanje "q2" false
    |> dodaj_prehod "q0" '0' "q0"
    |> dodaj_prehod "q1" '0' "q1"
    |> dodaj_prehod "q2" '0' "q2"
    |> dodaj_prehod "q0" '1' "q1"
    |> dodaj_prehod "q1" '1' "q2"
    |> dodaj_prehod "q2" '1' "q0"
)
(* val enke_deljive_s_3 : DFA.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ### Izpis avtomata
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dot_of_dfa : DFA.t -> string`, ki vrne zapis avtomata v
 formatu `dot`. Na ta način si lahko avtomat ogledate s programom
 [GraphViz](https://graphviz.org) ali kar v [spletnem
 pregledovalniku](https://www.devtoolsdaily.com/graphviz/).
[*----------------------------------------------------------------------------*)

let dot_of_dfa (t: DFA.t) = 
  let zac = DFA.zacetno_stanje t in
  let seznama =
    let rec aux acc1 acc2 sez =
      match sez with
      | [] -> (acc1,acc2)
      | x::xs when DFA.je_sprejemno_stanje t x -> aux (x ^ " " ^ acc1) acc2 xs
      | x::xs -> aux acc1 (x ^ " " ^ acc2) xs
    in aux "" "" (DFA.seznam_stanj t)
  in
  let validNodes = fst seznama in
  let invalidNodes = snd seznama in 
  let validNodeString =
    if validNodes <> "" then
      Printf.sprintf "node [shape = doublecircle]; %s;\n" (String.trim validNodes)
    else
      ""
  in
  let invalidNodeString =
    if invalidNodes <> "" then
      Printf.sprintf "node [shape = circle]; %s;\n" (String.trim invalidNodes)
    else
      ""
  in
  let strPrehod ((zac : DFA.stanje),(prehodno : char),(koncno : DFA.stanje)) : string = 
    Printf.sprintf "%s -> %s [label=\"%c\"];\n  " zac koncno prehodno in
  let prehodi = List.fold_left (fun acc (a,b,c)-> acc ^ strPrehod (a,b,c)) "" (DFA.seznam_prehodov t) in
  let s = Printf.sprintf "digraph DFA {
  rankdir=LR;
  size=\"8,5\"
  %s;
  %s;
  \"\" [shape = none];
  \"\" -> %s;
  %s}" validNodeString invalidNodeString zac prehodi in
  s

let () = enke_deljive_s_3 |> dot_of_dfa |> print_endline

(*----------------------------------------------------------------------------*
 ### Sprejemanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `dfa_sprejema : DFA.t -> string -> bool`, ki preveri, ali
 avtomat sprejme podani niz.
[*----------------------------------------------------------------------------*)

let dfa_sprejema (dfa : DFA.t) (s : string) : bool =
  let zac = DFA.zacetno_stanje dfa in
  let n = String.length s in
  let check st =
    let aux cur i =
      match DFA.prehodna_funkcija dfa cur st.[i] with
      | None -> ""
      | Some x -> x
    in let rec go cur i =
      if i=n then
        DFA.je_sprejemno_stanje dfa cur
      else
        let newState = aux cur i in
        if newState <> "" then
          go newState (i+1)
        else false
    in go zac 0 
  in check s


let nizi =
  let razsiri nizi = List.map ((^) "0") nizi @ List.map ((^) "1") nizi in
  let razsiri_zadnjega =
    function
    | [] -> []
    | (zadnji :: _) as vsi -> razsiri zadnji :: vsi
  in
  let rec loop n vsi =
    if n = 0 then
      vsi |> List.rev |> List.flatten
    else
      loop (n - 1) (razsiri_zadnjega vsi)
  in
  loop 5 [[""]]
(* val nizi : string list =
  [""; "0"; "1"; "00"; "01"; "10"; "11"; "000"; "001"; "010"; "011"; "100";
   "101"; "110"; "111"; "0000"; "0001"; "0010"; "0011"; "0100"; "0101";
   "0110"; "0111"; "1000"; "1001"; "1010"; "1011"; "1100"; "1101"; "1110";
   "1111"; "00000"; "00001"; "00010"; "00011"; "00100"; "00101"; "00110";
   "00111"; "01000"; "01001"; "01010"; "01011"; "01100"; "01101"; "01110";
   "01111"; "10000"; "10001"; "10010"; "10011"; "10100"; "10101"; "10110";
   "10111"; "11000"; "11001"; "11010"; "11011"; "11100"; "11101"; "11110";
   "11111"] *)

let primer_dfa = List.filter (dfa_sprejema enke_deljive_s_3) nizi
(* val primer_dfa : string list =
  [""; "0"; "00"; "000"; "111"; "0000"; "0111"; "1011"; "1101"; "1110";
   "00000"; "00111"; "01011"; "01101"; "01110"; "10011"; "10101"; "10110";
   "11001"; "11010"; "11100"] *)

(*----------------------------------------------------------------------------*
 ## Nedeterministični končni avtomati
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nedeterministični končni avtomati (_nondeterministic finite automaton_ oz. NFA)
 se od determinističnih razlikujejo v dveh pogledih:
 - dopuščajo prazne prehode med stanji, torej prehode, ki se zgodijo brez branja
 simbola iz niza,
 - iz enega stanja lahko obstaja več prehodov za isti simbol.

 Prehodno funkcijo $\delta$ tako definiramo kot $\delta : Q \times (\Sigma \cup
 \{ \varepsilon \}) \rightarrow \mathcal{P}(Q)$, kjer simbol $\varepsilon$
 predstavlja prazen prehod, $\mathcal{P}(Q)$ pa je potenčna množica množice
 stanj $Q$. Vsak deterministični končni avtomat je tudi nedeterminističen končni
 avtomat, velja pa tudi obratno. Za vsak nedeterministični končni avtomat lahko
 ustvarimo ustrezen deterministični končni avtomat, ki sprejema iste nize
 (namig: za stanja ustreznega DFA vzamemo podmnožice stanj NFA).

 Avtomat sprejme niz, če obstaja pot, ki ji sledimo po prehodih z zaporednimi
 znaki niza ali praznih prehodih, tako da začnemo v začetnem stanju in končamo v
 enem od sprejemnih stanj.

 Na primer, vzemimo avtomat, ki sprejema nize, sestavljene iz ničel in enic, v
 katerih je ali število enic ali število ničel deljivo s 3. Tak avtomat je
 sestavljen iz začetnega stanja, s praznim prehodom v eno kopijo prejšnjega
 avtomata ter še enim praznim prehodom v drugo kopijo, v kateri zamenjamo vlogi
 znakov.

 ![NFA](slike/nfa.png)

 Kot pri primeru za deterministični avtomat je avtomat definiran nad abecedo
 $\Sigma = \{ 0, 1\}$, stanj je tokrat sedem, poleg začetnega še po tri v vsaki
 _kopiji_ avtomata iz prvega primera. Tako je $Q = \{ q_0, q_{00}, q_{01},
 q_{02}, q_{10}, q_{11}, q_{12} \}$. Začetno stanje je $q_0$, sprejemni pa sta
 $q_{00}$ in $q_{10}$. Prehodna funkcija je definirana kot:

 | $\delta$   | `0`          | `1`          | $\varepsilon$ |
 | ---------- | ------------ | ------------ | ------------- |
 | $q_0$      | $\emptyset$  | $\emptyset$  | $\{q_{00}\}$  |
 | $q_0$      | $\emptyset$  | $\emptyset$  | $\{q_{10}\}$  |
 | $q_{00}$   | $\{q_{01}\}$ | $\{q_{00}\}$ | $\emptyset$   |
 | $q_{01}$   | $\{q_{02}\}$ | $\{q_{01}\}$ | $\emptyset$   |
 | $q_{02}$   | $\{q_{00}\}$ | $\{q_{02}\}$ | $\emptyset$   |
 | $q_{10}$   | $\{q_{10}\}$ | $\{q_{11}\}$ | $\emptyset$   |
 | $q_{11}$   | $\{q_{11}\}$ | $\{q_{12}\}$ | $\emptyset$   |
 | $q_{12}$   | $\{q_{12}\}$ | $\{q_{10}\}$ | $\emptyset$   |

 Če avtomat prejme niz `10011`, bo prehajal skozi stanja:
 - Začetno stanje: $q_0$
 - Pred branjem znaka `1` se lahko po praznih prehodih premakne v stanji
 $q_{00}$ in $q_{10}$, tako da so njegova možna stanja $\{q_0, q_{00}, q_{10}\}$
 - Prebere `1`: vsa tri možna stanja se premaknejo, če se lahko (stanje $q_0$
 nima možnega premika), tako so možna stanja $\{q_{01}, q_{10}\}$
 - Pred branjem novega znaka se zopet lahko vsa možna stanja premaknejo po
 praznih prehodih, a ker teh ni, ostaneta možni stanji $\{q_{01}, q_{10}\}$
 - Prebere `0`: po prehodih so možna stanja $\{q_{01}, q_{11}\}$
 - Prazni premiki in branje `0`: možna stanja $\{q_{02}, q_{11}\}$
 - Prazni premiki in branje `1`: možna stanja $\{q_{02}, q_{12}\}$
 - Prazni premiki in branje `1`: možna stanja $\{q_{02}, q_{10}\}$
 - Prazni premiki: možna stanja ostanejo $\{q_{02}, q_{10}\}$

 Ker je stanje $q_{10}$ sprejemno, avtomat niz sprejme.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Modul `NFA`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Nedeterministične avtomate predstavimo s signaturo `NFA_SIG`, podobno zgornji:
[*----------------------------------------------------------------------------*)

module type NFA_SIG = sig
  type stanje = string
  type t

  (* Funkcije za grajenje *)
  val ustvari : stanje -> bool -> t
  val dodaj_stanje : stanje -> bool -> t -> t
  val dodaj_prehod : stanje -> char -> stanje -> t -> t
  val dodaj_prazen_prehod : stanje -> stanje -> t -> t

  (* Funkcije za poizvedovanje *)
  val seznam_stanj : t -> stanje list
  val zacetno_stanje : t -> stanje
  val je_sprejemno_stanje : t -> stanje -> bool
  val prehodna_funkcija : t -> stanje -> char option -> stanje list
  val seznam_prehodov : t -> (stanje * char option * stanje) list
end

(*----------------------------------------------------------------------------*
 Napišite modul `NFA`, ki zadošča zgornji signaturi.
[*----------------------------------------------------------------------------*)

module NFA : NFA_SIG = struct
  type stanje = string
  type stanja = (stanje * bool) list
  type prehod = (stanje * char option * stanje)
  type prehodi = prehod list
  type t = (stanja * prehodi) 

  let ustvari (s : stanje) (v : bool) : t = ([(s,v)], [])
  let dodaj_stanje (s : stanje) (v : bool) (nfa : t) : t =
    let nova_stanja = (s,v) :: (fst nfa) in
    let preh = snd nfa in
    (nova_stanja, preh)
  let dodaj_prehod (zac : stanje) (prehodno : char) (koncno : stanje) (nfa : t) : t =
    let sezStanj = fst nfa in
    let znak = Some prehodno in
    let sezPrehodov = (zac,znak,koncno) :: (snd nfa) in
    (sezStanj, sezPrehodov)

  let dodaj_prazen_prehod (zac : stanje) (konc : stanje) (nfa : t) : t = 
    let sezStanj = fst nfa in
    let prehod = (zac, None, konc) in
    let prehodi = prehod :: snd nfa in
    (sezStanj,prehodi)

  let seznam_stanj (nfa : t) : stanje list = List.map (fun (x,y) -> x) (fst nfa)
  let zacetno_stanje (nfa : t) : stanje = 
    let rec aux cur s =
      match s with
      | [] -> cur
      | (st, v)::xs -> aux st xs
    in aux "" (fst nfa)
  let je_sprejemno_stanje (nfa : t) (s : stanje) = 
    let rec aux sez =
      match sez with
      | [] -> false
      | (stan,v)::xs when s=stan -> v
      | x::xs -> aux xs
    in aux (fst nfa)
  let prehodna_funkcija (nfa : t) (s : stanje) (ch : char option) : stanje list = 
    let sez = snd nfa in
    let aux acc (zac, prehod, konc) =
      if (zac = s && ch = prehod) then
        konc :: acc
      else acc in
    List.fold_left (aux) [] sez
  let seznam_prehodov (nfa : t) = snd nfa
end

(*----------------------------------------------------------------------------*
 Primer zgornjega avtomata bi lahko zapisali kot:
[*----------------------------------------------------------------------------*)

let enke_ali_nicle_deljive_s_3 = NFA.(
    ustvari "q0" false
    |> dodaj_stanje "q00" true
    |> dodaj_stanje "q01" false
    |> dodaj_stanje "q02" false
    |> dodaj_prehod "q00" '0' "q01"
    |> dodaj_prehod "q01" '0' "q02"
    |> dodaj_prehod "q02" '0' "q00"
    |> dodaj_prehod "q00" '1' "q00"
    |> dodaj_prehod "q01" '1' "q01"
    |> dodaj_prehod "q02" '1' "q02"
    |> dodaj_stanje "q10" true
    |> dodaj_stanje "q11" false
    |> dodaj_stanje "q12" false
    |> dodaj_prehod "q10" '1' "q11"
    |> dodaj_prehod "q11" '1' "q12"
    |> dodaj_prehod "q12" '1' "q10"
    |> dodaj_prehod "q10" '0' "q10"
    |> dodaj_prehod "q11" '0' "q11"
    |> dodaj_prehod "q12" '0' "q12"
    |> dodaj_prazen_prehod "q0" "q00"
    |> dodaj_prazen_prehod "q0" "q10"
)
(* val enke_ali_nicle_deljive_s_3 : NFA.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ### Izpis avtomata
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dot_of_nfa : NFA.t -> string`, ki vrne zapis avtomata v
 formatu `dot`.
[*----------------------------------------------------------------------------*)

let dot_of_nfa (nfa : NFA.t) : string = 
  let zac = NFA.zacetno_stanje nfa in
  let seznama =
    let rec aux acc1 acc2 sez =
      match sez with
      | [] -> (acc1,acc2)
      | x::xs when NFA.je_sprejemno_stanje nfa x -> aux (x ^ " " ^ acc1) acc2 xs
      | x::xs -> aux acc1 (x ^ " " ^ acc2) xs
    in aux "" "" (NFA.seznam_stanj nfa)
  in
  let validNodes = fst seznama in
  let invalidNodes = snd seznama in 
  let validNodeString =
    if validNodes <> "" then
      Printf.sprintf "node [shape = doublecircle]; %s;\n  " (String.trim validNodes)
    else
      ""
  in
  let invalidNodeString =
    if invalidNodes <> "" then
      Printf.sprintf "node [shape = circle]; %s;" (String.trim invalidNodes)
    else
      ""
    in
  let nodes = validNodeString ^ invalidNodeString in
  let strPrehod (z,prehodno,konc) =
    let znak = match prehodno with
    | Some ch -> String.make 1 ch
    | None -> "ε" 
    in
    Printf.sprintf "%s -> %s [label=\"%s\"];\n  " z konc znak
  in
  let prehodi = List.fold_left (fun acc x -> strPrehod x ^ acc) "" (NFA.seznam_prehodov nfa) in
  let s = Printf.sprintf "digraph NFA {
  rankdir=LR;
  size=\"8,5\"
  %s
  \"\" [shape = none];
  \"\" -> %s;
  %s}" nodes zac prehodi in
  String.trim s
  
let () = enke_ali_nicle_deljive_s_3 |> dot_of_nfa |> print_endline

(*----------------------------------------------------------------------------*
 ### Sprejemanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `nfa_sprejema : NFA.t -> string -> bool`, ki preveri, ali
 avtomat sprejme podani niz.
[*----------------------------------------------------------------------------*)

let nfa_sprejema (nfa : NFA.t) (s : string) : bool = 
  let zac = NFA.zacetno_stanje nfa in
  let n = String.length s in
  let naslednjeNeobiskane all visited =
    (* S1 \ S2 *)
    let rec aux acc sez = 
      match sez with
      | x::xs when not (List.mem x visited) -> aux (x::acc) xs
      | x::xs -> aux acc xs
      | [] -> acc
    in 
    aux [] all
  in
  let rec go (cur : string) (i : int) (visited : string list) : bool =
    if i=n then
      if NFA.je_sprejemno_stanje nfa cur then
        true
      else
        let vsiNeobiskani = NFA.prehodna_funkcija nfa cur None in
        let naslednji = naslednjeNeobiskane vsiNeobiskani visited in
        let rec aux sez =
          match sez with
          | [] -> false
          | x::xs -> go x i (x::visited)
        in aux naslednji
    else
      let ch = Some s.[i] in
      let neprazni = NFA.prehodna_funkcija nfa cur ch in
      let prazni1 = NFA.prehodna_funkcija nfa cur None in
      let prazni = naslednjeNeobiskane prazni1 visited in
      let rec auxN sez =
        match sez with
        | [] -> false
        | x::xs ->
          if go x (i+1) [] then
            true
          else
            auxN xs
        in
      if auxN neprazni then 
        true
      else
        let rec auxP sez vis = 
          match sez with
          | [] -> false
          | x::xs ->
            if go x (i) (x::vis) = true then
              true
            else
              auxP xs (x::vis)
        in auxP prazni visited
        
  in
  go zac 0 []
      

let primer_nfa = List.filter (nfa_sprejema enke_ali_nicle_deljive_s_3) nizi
(* val primer_nfa : string list =
  [""; "0"; "1"; "00"; "11"; "000"; "111"; "0000"; "0001"; "0010"; "0100";
   "0111"; "1000"; "1011"; "1101"; "1110"; "1111"; "00000"; "00011"; "00101";
   "00110"; "00111"; "01001"; "01010"; "01011"; "01100"; "01101"; "01110";
   "10001"; "10010"; "10011"; "10100"; "10101"; "10110"; "11000"; "11001";
   "11010"; "11100"; "11111"] *)

(*----------------------------------------------------------------------------*
 ## Regularni izrazi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Regularni izrazi so formalni opisi jezikov (množic nizov) nad abecedo $\Sigma$.
 Uporabljajo se za kompaktno opisovanje vzorcev, ki jim morajo nizi ustrezati.
 Rečemo, da niz *ustreza* regularnemu izrazu, če ga lahko zgradimo z
 upoštevanjem pravil, ki jih določa ta izraz.

 Regularni izrazi so sestavljeni iz osnovnih elementov in operacij:

 - $\emptyset$ ne ustreza nobenemu jeziku,
 - $\varepsilon$ ustreza prazen niz,
 - za vsak znak $a \in \Sigma$, izrazu $a$ ustreza natanko natanko niz dolžine
 1, sestavljenem le iz znaka $a$,
 - *uniji* $r_1 \mid r_2$ ustreza vsem nizom, ki ustrezajo $r_1$ ali $r_2$,
 - *stiku* $r_1 r_2$ ustrezajo vsi nizi oblike $s_1 s_2$, kjer $s_1$ ustreza
 $r_1$ ter $s_2$ ustreza $r_2$,
 - *Kleenejevemu zaprtju* $r^*$ ustrezajo vsi nizi oblike $s_1 s_2 \cdots s_n$
 za nek $n$ (vključno s praznim nizom), kjer vsak izmed nizov $s_i$ ustreza $r$.

 Za primer si oglejmo regularni izraz $a^* b$. Ta izraz je sestavljen kot stik
 izrazov $a^*$ in $b$ in tako sprejme vse nize, ki se začnejo s poljubnim
 številom ponovitev (lahko nič) znaka $a$ in končajo z znakom $b$. Izrazu tako
 ustreza niz $aaab$, niz $aaabb$ pa ne. Nize, v katerih je število enk deljivo s
 tri, bi lahko opisali z regularnim izrazom $0^*( 10^*10^*10^* )^*$.

 Regularne izraze bomo implementirali z algebrajskim tipom, ki ima za
 konstruktorje zgoraj naštete osnovne elemente in operacije na regularnih
 izrazih.
[*----------------------------------------------------------------------------*)

type regex =
  | Empty
  | Eps
  | Char of char
  | Union of regex * regex
  | Concat of regex * regex
  | Star of regex

let re_enke_deljive_s_3 =
  let poljubno_nicel = Star (Char '0') in
  let enka_in_poljubno_nicel = Concat (Char '1', poljubno_nicel) in
  Concat (poljubno_nicel, Star (Concat (Concat (enka_in_poljubno_nicel, enka_in_poljubno_nicel), enka_in_poljubno_nicel)))
(* val re_enke_deljive_s_3 : regex =
  Concat (Star (Char '0'),
   Star
    (Concat
      (Concat (Concat (Char '1', Star (Char '0')),
        Concat (Char '1', Star (Char '0'))),
      Concat (Char '1', Star (Char '0'))))) *)

(*----------------------------------------------------------------------------*
 ### Izpisovanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `string_of_regex : Regex.t -> string`, ki regularni izraz
 predstavi z nizom. Pri tem poskusite zapisati čim manj oklepajev, upoštevaje
 to, da ima Kleenejevo zaprtje najvišjo prioriteto, sledi stik, nato pa unija.
 Poleg tega sta stik in unija asociativni operaciji.
[*----------------------------------------------------------------------------*)

let string_of_regex (t : regex) : string = 
  let rec aux (reg : regex) : string = 
    match reg with
    | Empty -> "{}"
    | Eps -> ""
    | Char c -> String.make 1 c 
    | Union (reg1, reg2) ->
      let r1 = aux reg1 in
      let r2 = aux reg2 in
      if (r1 = "{}") && (r2 = "{}") then
        "{}"
      else if (r1 = "{}" || r1="") then
        r2
      else if (r2 = "{}" || r2 = "") then
        r1
      else if String.length r1 = 1 && String.length r2 = 1 then
        r1^r2
      else if String.length r1 = 1 then
        r1^"|(" ^ r2 ^ ")"
      else if String.length r2 = 1 then
        "(" ^ r1 ^ ")|" ^ r2 
      else
        Printf.sprintf "(%s)|(%s)" r1 r2
    | Concat (reg1, reg2) ->
      let r1 = aux reg1 in
      let r2 = aux reg2 in
      if r1 = "{}" || r2 = "{}" then
        "{}"
      else r1 ^ r2
    | Star reg ->
      let r = aux reg in
      if (r="{}") || (r="") then
        ""
      else if String.length(r) = 1 then
        r^"*"
      else
        Printf.sprintf "(%s)*" r
  in
  aux t



let primer_regex_1 = string_of_regex re_enke_deljive_s_3
(* val primer_regex_1 : string = "0*(10*10*10*)*" *)

(*----------------------------------------------------------------------------*
 ### Sprejeti nizi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `regex_sprejema: regex -> string -> bool`, ki preveri, ali
 dan niz ustreza regularnemu izrazu.
[*----------------------------------------------------------------------------*)

let regex_sprejema (r : regex) (s : string) : bool =
  let memo = Hashtbl.create 256 in

  let rec aux r s =
    match Hashtbl.find_opt memo (r,s) with
    | Some k -> k
    | None ->
      let res = 
        match r with
        | Empty -> false
        | Eps -> s = ""
        | Char c -> String.length s == 1 && s.[0]=c
        | Union (r1,r2) -> aux r1 s || aux r2 s 
        | Concat (r1,r2) ->
          let n = String.length s in
          let rec split_at i =
            if i>n then false
            else
              let s1 = String.sub s 0 i in
              let s2 = String.sub s i (n-i) in
              (aux r1 s1 && aux r2 s2) || split_at (i+1) in
          split_at 0
        | Star r1 ->
          if s = "" then true
          else
            let n = String.length s in
            let rec try_at i =
              if i>n then false
              else if i = 0 then try_at 1
              else
                let prefix = String.sub s 0 i in
                let rest = String.sub s i (n-i) in
                (aux r1 prefix) && (aux (Star r1) rest) || (try_at (i+1))
              in 
            try_at 0
      in 
      Hashtbl.add memo (r,s) res;
      res
    in 
  aux r s


let primer_regex_2 = regex_sprejema re_enke_deljive_s_3 "10011"
(* val primer_regex_2 : bool = true *)

let primer_regex_3 = regex_sprejema re_enke_deljive_s_3 "100111"
(* val primer_regex_3 : bool = false *)

(*----------------------------------------------------------------------------*
 ### Od regularnega izraza do avtomata
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Izkaže se, da med regularnimi izrazi in nedeterminističnimi končnimi avtomati
 obstaja ekvivalenca. Za vsak izraz obstaja ustrezen avtomat, ki sprejema iste
 nize in obratno. Mi bomo ekvivalenco pokazali le v eno stran tako, da bomo za
 vsak konstruktor regularnih izrazov definirali ustrezno konstrukcijo na končnih
 avtomatih.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte avtomat `prazen_nfa: NFA.t`, ki ne sprejme nobenega niza.
[*----------------------------------------------------------------------------*)


let prazen_nfa : NFA.t = NFA.ustvari "p" false

let primer_regex_4 = List.filter (nfa_sprejema prazen_nfa) nizi
(* val primer_regex_4 : string list = [] *)

(*----------------------------------------------------------------------------*
 Definirajte avtomat `epsilon_nfa: NFA.t`, ki sprejme natanko prazen niz.
[*----------------------------------------------------------------------------*)

let epsilon_nfa  = NFA.ustvari "p" true 

let primer_regex_5 = List.filter (nfa_sprejema epsilon_nfa) nizi
(* val primer_regex_5 : string list = [""] *)

(*----------------------------------------------------------------------------*
 Definirajte funkcijo `znak_nfa: char -> NFA.t`, ki vrne avtomat, ki sprejme
 natanko niz dolžine ena z znakom v argumentu.
[*----------------------------------------------------------------------------*)

let znak_nfa znak = 
  let nfa = NFA.(
    ustvari "p" false
    |> dodaj_stanje "q" true
    |> dodaj_prehod "p" znak "q"
  ) in
  nfa

let primer_regex_6 = List.filter (nfa_sprejema (znak_nfa '0')) nizi
(* val primer_regex_6 : string list = ["0"] *)

(*----------------------------------------------------------------------------*
 Definirajte funkcijo `unija_nfa: NFA.t -> NFA.t -> NFA.t`, ki vrne avtomat, ki
 sprejme nize sprejete s katerim koli izmed avtomatov v argumentih.
[*----------------------------------------------------------------------------*)


(* pomozne funkcije za delo z nfa; rename nfa naredi vsa vozlisca unikatna zato *)
(* da ni sporov ko uporabimo unijo in zdruzimo prehode *)
(* join nfa ... self-explanatory? ([(e1,v1)],[(e1,c,e1)]) ([(e2,v2)],[(e2,c2,e2)]) *)
(* bo vrnil ([(e1,v1),(e2,v2)], [(e1,c,e1),(e2,c2,e2)]), pac zduzi ju skupaj *)
let counter = ref 0

let new_state () = 
  let n = !counter in
  incr counter;
  "p" ^ string_of_int n

let rename_nfa (nfa : NFA.t) : NFA.t =
  (* update all node names and store the relation in assoc *
  [* list and make a mapping between them *)
  let sez = NFA.seznam_stanj nfa in
  let assocLst = List.map (fun oldName -> (oldName, new_state ())) sez in
  let mapping oldName = List.assoc oldName assocLst in

  (* Old and new start are required to make acc for fold left *)
  let oldStart = NFA.zacetno_stanje nfa in
  let newStart = mapping oldStart in

  (* transform all old states, aux is starting acc *)
  let aux = NFA.ustvari newStart (NFA.je_sprejemno_stanje nfa oldStart) in
  let newNFA = List.fold_left (
    fun (acc : NFA.t) oldName -> 
      if oldName = oldStart then acc
      else
        NFA.dodaj_stanje (mapping oldName) (NFA.je_sprejemno_stanje nfa oldName) acc
    ) aux sez in

  (* transform all transition *)
  let trans = NFA.seznam_prehodov nfa in
  let newTrans = List.map (fun (oldS, ch, oldE) ->
    let newS = mapping oldS in
    let newE = mapping oldE in 
    (newS, ch, newE)
    ) trans in
  
  List.fold_left (
    fun acc (s,c,e) -> 
      match c with
      | Some x -> NFA.dodaj_prehod s x e acc
      | None -> NFA.dodaj_prazen_prehod s e acc
  ) newNFA newTrans

let join_nfa (nfa1 : NFA.t) (nfa2: NFA.t) : NFA.t = 
  (* ta funkcija naj bi se klicala samo za *)
  (* nfa-ja ki nimata skupnih imen zato ni problemov *)
  (* ce enostavno zdruzimo seznama *)
  (* vredno omeniti da bo nfa.zacetno_stanje vrnilo zacetno stanje nfa1 *)

  (* dobimo seznam stanj (stanje, valid) list *)
  let stanja= NFA.seznam_stanj nfa2 in
  let stanInVred = List.map (fun st -> (st, NFA.je_sprejemno_stanje nfa2 st)) stanja in

  (* dodamo jih nfa1 *)
  let nfaNew = List.fold_left (fun acc (st,v) ->
    NFA.dodaj_stanje st v acc) nfa1 stanInVred in
  
  (* dobimo seznam prehodov nfa2 in jih dodamo nfa1 *)
  let prehodi = NFA.seznam_prehodov nfa2 in
  List.fold_left (fun acc (z,c,k) ->
    match c with
    | Some x -> NFA.dodaj_prehod z x k acc
    | None -> NFA.dodaj_prazen_prehod z k acc) nfaNew prehodi

let unija_nfa (nfa11 : NFA.t) (nfa22 : NFA.t) : NFA.t =
  (* ustvarimo novo zacento vozlisce *)
  let nfa'' = NFA.ustvari (new_state ()) false in
  let start = NFA.zacetno_stanje nfa'' in
  
  (* osvezimo imena nfa1,2 da so imena vozlisc disjunktna*)
  let nfa1 = rename_nfa nfa11 in
  let nfa2 = rename_nfa nfa22 in
  let start1 = NFA.zacetno_stanje nfa1 in
  let start2 = NFA.zacetno_stanje nfa2 in

  (* ustvarimo zdruzen nfa, ki ima za zacetno stanje *)
  (* ravno zacetno vozlisce iz zacetka *)
  let nfa' = join_nfa nfa1 nfa2 in
  let nfa = join_nfa nfa'' nfa' in

  (* dodamo prazen prehod iz start v start 1,2 *)
  let unija = NFA.dodaj_prazen_prehod start start1 (
    NFA.dodaj_prazen_prehod start start2 nfa
  ) in
  unija
  
let primer_regex_7 = List.filter (nfa_sprejema (unija_nfa epsilon_nfa (znak_nfa '0'))) nizi
(* val primer_regex_7 : string list = [""; "0"] *)

(*----------------------------------------------------------------------------*
 Definirajte funkcijo `stik_nfa: NFA.t -> NFA.t -> NFA.t`. Vrnjeni avtomat
 sprejme nize sestavljene iz stika prvega dela, ki ga sprejme avtomat v prvem
 argumentu, in drugega dela, ki ga sprejme avtomat v drugem argumentu.
[*----------------------------------------------------------------------------*)

let stik_nfa (nfa1' : NFA.t) (nfa2' : NFA.t) : NFA.t = 
  let nfa1 = rename_nfa nfa1' in
  let nfa2 = rename_nfa nfa2' in
  let start1 = NFA.zacetno_stanje nfa1 in
  let start1Bool = NFA.je_sprejemno_stanje nfa1 start1 in 
  let start2 = NFA.zacetno_stanje nfa2 in
  
  (* ustvarimo nov nfa s stanjema start 1,2 *)
  let zac = NFA.ustvari start1 start1Bool in

  (* dodamo vsa stanja nfa 2 *)
  let stanja2 = NFA.seznam_stanj nfa2 in
  let nfaStanja2 : NFA.t = List.fold_left (fun acc str ->
    let vred = NFA.je_sprejemno_stanje nfa2 str in
    let temp = NFA.dodaj_stanje str vred acc in
    temp
  ) zac stanja2 in

  (* dodamo vsa stanja nfa 1 in ce sprejemna 
  dodamo se prazen prehod na start 2*)

  let aux acc str =
    let vred = NFA.je_sprejemno_stanje nfa1 str in
    (* zacetno smo ze dodali *)  
    if vred && (str <> start1) then
      let temp' = NFA.dodaj_stanje str false acc in
      let temp = NFA.dodaj_prazen_prehod str start2 temp' in
      temp
    else if vred then
      let temp = NFA.dodaj_prazen_prehod start1 start2 acc in
      temp
    else if (str <> start1) then
      let temp = NFA.dodaj_stanje str false acc in
      temp
    else
      acc
    in

  let nfaStanja1 = List.fold_left (aux) nfaStanja2 (NFA.seznam_stanj nfa1) in
  
  (* Dodamo vse prehode *)

  let prehodi1 = NFA.seznam_prehodov nfa1 in
  let prehodi2 = NFA.seznam_prehodov nfa2 in
  
  let aux' acc (z, (ch : char option), k) : NFA.t =
    match ch with
    | Some x -> NFA.dodaj_prehod z x k acc
    | None -> NFA.dodaj_prazen_prehod z k acc

  in List.fold_left (aux') nfaStanja1 (prehodi1 @ prehodi2)

let primer_regex_8 = List.filter (nfa_sprejema (stik_nfa (znak_nfa '0') (znak_nfa '1'))) nizi
(* val primer_regex_8 : string list = ["01"] *)

(*----------------------------------------------------------------------------*
 Definirajte funkcijo `kleenejevo_zaprtje_nfa: NFA.t -> NFA.t`. Vrnjeni avtomat
 naj sprejme nize, ki jih dobimo s poljubnim ponavljanjem nizov, ki jih sprejme
 avtomat v argumentu.
[*----------------------------------------------------------------------------*)

let kleenejevo_zaprtje_nfa (nfa'' : NFA.t) : NFA.t = 
  (* naredimo nov avtomat, ki ima nov start ki pelje do starega 
    in nov konec *)
  let nfa' = rename_nfa nfa'' in
  let oldStart = NFA.zacetno_stanje nfa' in
  let newEnd = new_state () in
  let newStart = new_state () in
  let oldStartBool = NFA.je_sprejemno_stanje nfa' oldStart in
  let newNfa' = NFA.(
    ustvari newStart false
    |> dodaj_stanje oldStart false
    |> dodaj_stanje newEnd true
    |> dodaj_prazen_prehod newStart oldStart
    |> dodaj_prazen_prehod newStart newEnd
  ) in
  let newNfa = 
    if oldStartBool then NFA.dodaj_prazen_prehod oldStart newEnd newNfa'
    else newNfa'
  in

  (* Pretvori stanja *)
  let stanja = NFA.seznam_stanj nfa' in
  let aux acc st =
    if st = oldStart then acc
    else
      let vred = NFA.je_sprejemno_stanje nfa' st in
      let temp = NFA.dodaj_stanje st false acc in
      if not vred then temp
      else
        let temp1 = NFA.dodaj_prazen_prehod st oldStart temp in
        NFA.dodaj_prazen_prehod st newEnd temp1
  
  in 
  let nfaStanja = List.fold_left aux newNfa stanja in

  (* Dodamo prehode *)
  let prehodi = NFA.seznam_prehodov nfa' in
  let auxP acc (z, ch, k) =
    match ch with
    | Some x -> NFA.dodaj_prehod z x k acc
    | None -> NFA.dodaj_prazen_prehod z k acc
  in List.fold_left auxP nfaStanja prehodi

let primer_regex_9 = List.filter (nfa_sprejema (kleenejevo_zaprtje_nfa (znak_nfa '0'))) nizi
(* val primer_regex_9 : string list = [""; "0"; "00"; "000"; "0000"; "00000"] *)

(*----------------------------------------------------------------------------*
 Zgoraj definirane funkcije združite v definicijo funkcijo `nfa_of_regex: regex
 -> NFA.t`, ki danemu regularnemu izrazu priredi `NFA`, ki sprejme isti jezik.
[*----------------------------------------------------------------------------*)

let rec nfa_of_regex (r : regex) : NFA.t = 
  match r with
  | Empty -> prazen_nfa
  | Eps -> epsilon_nfa
  | Char znak -> znak_nfa znak
  | Union (r1,r2) -> unija_nfa (nfa_of_regex r1) (nfa_of_regex r2)
  | Concat (r1,r2) -> stik_nfa (nfa_of_regex r1) (nfa_of_regex r2)
  | Star r1 -> kleenejevo_zaprtje_nfa (nfa_of_regex r1)

let primer_regex_10 = List.filter (nfa_sprejema (nfa_of_regex re_enke_deljive_s_3)) nizi
(* val primer_regex_10 : string list =
  [""; "0"; "00"; "000"; "111"; "0000"; "0111"; "1011"; "1101"; "1110";
   "00000"; "00111"; "01011"; "01101"; "01110"; "10011"; "10101"; "10110";
   "11001"; "11010"; "11100"] *)

let primer_regex_11 = List.filter (regex_sprejema re_enke_deljive_s_3) nizi
(* val primer_regex_11 : string list =
  [""; "0"; "00"; "000"; "111"; "0000"; "0111"; "1011"; "1101"; "1110";
   "00000"; "00111"; "01011"; "01101"; "01110"; "10011"; "10101"; "10110";
   "11001"; "11010"; "11100"] *)


