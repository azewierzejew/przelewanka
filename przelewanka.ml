(* Autor: Antoni Żewierżejew *)
(* Reviewer: Jakub Nowak     *)

(* zwraca NWD liczb a i b *)
let rec nwd a b =
    if a < b then nwd b a else
    if b = 0 then a else
    nwd b (a mod b)

open Array

(* wyjątek używany do szybkiego kończenia pętli, gdy wynik zostanie znaleziony *)
exception Znalezione of int

let przelewanka tablica =
    let n = length tablica in
    let pojemnosc = map fst tablica in
    let stan_koncowy = map snd tablica in
    
    if n = 0 then 0 else
    if not (fold_left (fun a (x, y) -> a || (y = 0) || (y = x)) false tablica) then -1 else
    let nwd_pojemnosc = fold_left nwd 0 pojemnosc in
    let nwd_koncowe = fold_left nwd 0 stan_koncowy in
    (* jeśli nwd_pojemnosc = 0 to wszystkie pojemności są równe 0, a wtedy od razu mamy wynik *)
    if nwd_pojemnosc = 0 then 0 else
    if nwd_koncowe mod nwd_pojemnosc <> 0 then -1 else
    
    let kolejka = Queue.create () in
    let hashmapa = Hashtbl.create 1000000 in
    
    let napelnij nr stan = 
        let wynik = copy stan in
        wynik.(nr) <- pojemnosc.(nr);
        wynik in
    
    let oproznij nr stan = 
        let wynik = copy stan in
        wynik.(nr) <- 0;
        wynik in
    
    let przelej zrodlo cel stan =
        let wynik = copy stan in
        let ilosc = min stan.(zrodlo) (pojemnosc.(cel) - stan.(cel)) in
        wynik.(zrodlo) <- wynik.(zrodlo) - ilosc;
        wynik.(cel) <- wynik.(cel) + ilosc;
        wynik in
    
    let dodaj stan glebokosc =
        if not (Hashtbl.mem hashmapa (to_list stan)) then 
        begin
            Hashtbl.add hashmapa (to_list stan) ();
            Queue.push (stan, glebokosc + 1) kolejka
        end in
            
    dodaj (make n 0) (-1);
    let it = ref 0 in
    try
        while not (Queue.is_empty kolejka) do
            let (stan, glebokosc) = Queue.pop kolejka in
            (* Printf.printf "%d %d\n" glebokosc !it; flush stdout; it:= !it+1; *)
            if stan = stan_koncowy then raise (Znalezione glebokosc) else
            for i = 0 to n - 1 do
                dodaj (napelnij i stan) glebokosc;
                dodaj (oproznij i stan) glebokosc;
                for j = 0 to n - 1 do
                    dodaj (przelej i j stan) glebokosc
                done
            done
        done;
        -1
    with Znalezione wynik -> wynik
