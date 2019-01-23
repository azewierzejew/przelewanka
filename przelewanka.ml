(* Autor: Antoni Żewierżejew *)
(* Reviewer: Jakub Nowak     *)

(* zwraca NWD liczb a i b, gdzie nwd a 0 = a, dla a, b >= 0 *)
let rec nwd a b =
    if a < b then nwd b a else
    if b = 0 then a else
    nwd b (a mod b)

open Array

(* wyjątek używany do szybkiego kończenia pętli, gdy wynik zostanie znaleziony *)
exception Znalezione of int

(*
    Jest dane n szklanek, ponumerowanych od 1 do n, o pojemnościach odpowiednio x1, x2, ..., xn.
    Początkowo wszystkie szklanki są puste. Można wykonywać następujące czynności:
    - nalać do wybranej szklanki do pełna wody z kranu, 
    - wylać całą wodę z wybranej szklanki do zlewu, 
    - przelać wodę z jednej szklanki do drugiej — jeżeli się zmieści, to przelewa się całą wodę,
      a jeżeli nie, to tyle żeby druga szklanka była pełna.

    Procedura przelewanka : (int * int) array → int, mając daną tablicę par liczb
    [|(x1, y1); (x2, y2); ...; (xn, yn)|] wyznaczy minimalną liczbę czynności potrzebnych
    do uzyskania ze stanu (0, 0, ..., 0) stanu (y1, y2, ..., yn).
    Jeżeli uzyskanie go nie jest możliwe, to poprawnym wynikiem jest -1. 

    Założenia 0 <= n, oraz 0 <= yi <= xi dla i = 1, 2, ..., n.
*)
let przelewanka tablica =
    (* zerowe kubełki nie wpływają na wynik *)
    let tablica = of_list (List.filter (fun (x, _) -> x <> 0) (to_list tablica)) in
    let n = length tablica in
    let pojemnosc = map fst tablica in
    let stan_koncowy = map snd tablica in
    
    if n = 0 then 0 else
    if not (fold_left (fun a (x, y) -> a || (y = 0) || (y = x)) false tablica) then -1 else
    let nwd_pojemnosc = fold_left nwd 0 pojemnosc in
    let nwd_koncowe = fold_left nwd 0 stan_koncowy in
    (* jest niezerowa liczba niezerowych kubełków, więc nwd_pojemnosc wyjdzie niezerowe *)
    if nwd_koncowe mod nwd_pojemnosc <> 0 then -1 else
    
    let kolejka = Queue.create () in
    let hashmapa = Hashtbl.create 2137 in
    
    (* funkcje pomocnicze wykonujące operację, zwracają None jeśli operacja nic nie da *)
    let napelnij nr stan = 
        if stan.(nr) = pojemnosc.(nr) then None else 
        let wynik = copy stan in
        wynik.(nr) <- pojemnosc.(nr);
        Some wynik in
    
    let oproznij nr stan = 
        if stan.(nr) = 0 then None else
        let wynik = copy stan in
        wynik.(nr) <- 0;
        Some wynik in
    
    let przelej zrodlo cel stan =
        if zrodlo = cel then None else
        let ilosc = min stan.(zrodlo) (pojemnosc.(cel) - stan.(cel)) in
        if ilosc = 0 then None else
        let wynik = copy stan in
        wynik.(zrodlo) <- wynik.(zrodlo) - ilosc;
        wynik.(cel) <- wynik.(cel) + ilosc;
        Some wynik in
    
    (* wrzuca nowy stan na hashtablicę i kolejkę, podnosi Znalezione gdy mamy wynik *)
    let dodaj stan_opcja glebokosc =
        match stan_opcja with
        | None -> ()
        | Some stan ->
            (* na Hashtbl wrzucamy listy, żeby modyfikacja tablicy [stan] nie wpływała na mapę *) 
            if not (Hashtbl.mem hashmapa (to_list stan)) then 
            begin
                if stan = stan_koncowy then raise (Znalezione glebokosc); 
                Hashtbl.add hashmapa (to_list stan) ();
                Queue.push (stan, glebokosc + 1) kolejka
            end in
    
    try
        dodaj (Some (make n 0)) 0;
        while not (Queue.is_empty kolejka) do
            let (stan, glebokosc) = Queue.pop kolejka in
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
