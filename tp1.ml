(* tp 1 * )
(* 1 *)
type 'a option = None | Some of 'a;;

let minmax l =
  let rec minmax' l emax emin=
    match l with
    |[] -> Some (emax, emin)
    |h::t -> minmax' t (max h emax) (min h emin)
  in match l with 
  |[] -> None
  |_ -> minmax' l (List.hd l) (List.hd l)
;;

let minmax2 l = List.fold_left 
    (fun (emax, emin) x -> (max x emax, min x emin))
    (List.hd l, List.hd l) 
    l 
  

(* 2 *)

let rec equals l1 l2 =
  match l1,l2 with
  |[],[] -> true
  |[], _|_, [] -> false
  |h1::t1,h2::t2 -> if (h1 = h2) then equals t1 t2 else false
;;

(* to_bin *)
let to_bin n =
  let rec to_bin' n l =
    match n with 
    |0 -> l 
    |n' -> to_bin' (n'/2) ((n' mod 2)::l)
  in match n with
  |0 -> [0]
  |_ -> to_bin' n []
;;
        

(* to_bin_dec *)

let to_bin_dec n =
  let rec to_bin_dec' l value =
    match l with 
    |[] -> value
    |h::t -> to_bin_dec' t (h*int_of_float (10. ** float_of_int(List.length t)) + value)
  in to_bin_dec' (to_bin n) 0
;;

   
(* last *)   

type 'a option = None | Some of 'a;;

let last l =
  let rec last' l tail =
    match l with
    |[] -> tail
    |h::t -> last' t (Some h)
  in last' l None
;;

let last2 l =
  match l with
  |[] -> None
  |[x] -> Some x
  |h::t -> last t 
;;

let rec last_two l =
  match l with
  |[] -> None
  |[x] -> None
  |[x;y] -> Some (x,y)
  |h::t -> last_two t
;;

     
(* flatten *)

type 'a node = One of 'a
             | Many of 'a node list ;;

let flatten enode =
  let rec flatten' enode acc =
    match enode with
    |[] -> acc
    |h::t -> 
        match h with
        |One x -> flatten' t (x::acc)
        |Many x -> flatten' t (flatten' x acc)
  in List.rev(flatten' enode [])
;;


        
(* compress *)

let compress l =
  let rec compress' l acc lastElt =
    match l with
    |[] -> acc
    |h::t -> if h = lastElt then compress' t acc h else compress' t (h::acc) h
  in List.(
      rev(
        compress' l [hd l] (hd l)
      )
    )
;;
        

(* pack *)

let pack l =
  let rec pack' l acc lastElt =
    match l with
    |[] -> acc
    |h::t -> 
        if h = lastElt 
        then pack' t ((h::(List.hd acc))::(List.tl acc)) h
        else pack' t ([h]::acc) h
  in List.rev( pack' (List.tl l) [[List.hd l]]  (List.hd l))
;;


let myzip l l' =
  let rec myzip' l l' acc =
    match l, l' with
    |_,[] | [], _ -> acc
    |h::t, h'::t' -> myzip' t t' ((h,h')::acc)
  in List.rev( myzip' l l' [])
;;
*)


(* fonction d ordre superieur *)

let header = ["Country"; "Capital"; "Population"];;

let data = [
  ["China"; "Beijing"; "1,427,647,786"];
  ["India"; "New Delhi"; "1,352,642,280"];
  ["United States"; "Washington, D.C."; "327,096,265"];
  ["Indonesia"; "Jakarta"; "267,670,543"];
  ["Pakistan"; "Islamabad"; "212,228,286"];
  ["Brazil"; "Brasilia"; "209,469,323"];
  ["Nigeria"; "Abuja"; "195,874,683"]
];;

  
let to_size l = 
  let rec to_size' l acc =
    match l with
    |[] -> acc
    |h::t -> to_size' t ((String.length h) :: acc)
  in List.rev(to_size' l [])
;;
(* ou *)
let to_size l = List.map (fun x -> String.length x) l;; 
        
let maxsize header data = 
  let rec maxsize' data acc =
    match data with
    |[] -> acc
    |h::t -> maxsize' t (List.map2 (fun x y -> max x y ) acc (to_size h))
  in maxsize' data (to_size header)
;;


let rec mainpad str n char =
  match n with
  |0 -> str
  |_ -> mainpad (str^char) (n-1) char 
;;

let pad str n = mainpad str n " ";;
        
    
let str_separator l = 
  let rec str_separator' l acc =
    match l with
    |[] -> ( acc^" |" )
    |[x] -> str_separator' [] (acc^(mainpad "" x "_"))
    |h::t -> str_separator' t (acc^(mainpad "" h "_")^" + ")
  in str_separator' l "| "
;;
          
          
let str_separator_border l = (mainpad " " (List.fold_left (+) 8 l) "_") ^ " ";;
  
  
let showtable header data = 
  Printf.printf " %s " (str_separator_border (maxsize header data));
  Printf.printf "\n";
  List.( 
    map2 (fun x y -> pad x y) header (map2 (-) (maxsize header data) (to_size header))
    |> iter (Printf.printf " | %s")
  );
  Printf.printf " |\n";
  Printf.printf " %s" (str_separator (maxsize header data));
  Printf.printf " \n";
  List.( iter (fun row ->
      map2 (fun x y -> pad x y) row (map2 (-) (maxsize header data) (to_size row))
      |> iter (Printf.printf " | %s") ;
      Printf.printf " |\n";
    ) data);
  Printf.printf " %s " (str_separator_border (maxsize header data));
;;
     
let showtable2 header data = 
  (* print hreader *)
  Printf.printf " %s " (str_separator_border (maxsize header data));
  Printf.printf "\n";
  List.( 
    map2 (fun x y -> pad x y) header (map2 (-) (maxsize header data) (to_size header))
    |> iter (Printf.printf " | %s")
  ); 
  Printf.printf " |\n";
  Printf.printf " %s" (str_separator (maxsize header data));
  Printf.printf " \n";
  (* print hreader end *)
  
  List.( 
    iter 
      (fun row ->
         map2 
           (fun x y -> pad x y) row (map2 (-) (maxsize header data) (to_size row))
         |> iter (Printf.printf " | %s") ;
         Printf.printf " |\n";
      ) data
  );
  
  (* print bottom *)
  Printf.printf " %s " (str_separator_border (maxsize header data));
;;      
  





let string_int str =  
  let rec string_int' lstr acc = 
    match lstr with
    |[] -> acc
    |h::t -> if h = ',' then string_int' t acc else string_int' t (acc^(String.make 1 h))
  in int_of_string (
    string_int' (List.init (String.length str) (String.get str)) ""
  )
;;
    
  
let filter data =
  List.filter (fun row -> (string_int (List.nth row 2)) > 300_000_000) data;;

  
let sort data = 
  List.sort (
    fun x y -> 
      match x, y with 
      |[c1;_;_],[c2;_;_] -> String.compare c1 c2 
      |_ -> 0
  ) data
;;
  
showtable header data;;

showtable header (sort (filter data));;

    
        
        
        
        
        