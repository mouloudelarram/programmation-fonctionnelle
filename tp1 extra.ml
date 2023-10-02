(* tp1 extra *)

type 'a fsystem =
  | File of 'a
  | Dir of 'a * ('a fsystem list)

let test =
  Dir ("root", [
      Dir ("bin", [File "ls"; File "cp"]);
      Dir ("users", [
          Dir ("usr1", [
              Dir ("Documents", [
                  File "1.doc";
                  File "2.tex";
                  File "3.xls";
                  Dir ("temp", [File "doc1.pdf"; File "temp.ps"])
                ]);
              Dir ("Photos", [File "1.png"; File "2.png"]);
              Dir ("Desktop", [])
            ]);
          Dir ("usr2", [
              Dir ("Downloads", [File "file1.exe"; File "file2.ml"])
            ])
        ])
    ])

let str_space n = 
  let rec str_space' n acc =
    match n with
    |0 -> acc
    |_ -> str_space' (n-1) acc^"|   "
  in str_space' n ""
;;
    

let ls dir =
  let rec print dir n =
    let rec print' l =
      match l with 
      |[] -> Printf.printf "";
      |h::t -> 
          Printf.printf "%s" (str_space n);
          print h (n+1);
          print' t; 
    in 
    match dir with 
    |File f -> Printf.printf "+--- %s\n" f
    |Dir (f, dir) -> 
        Printf.printf "+--- %s\n" f; 
        print' dir;
  in print dir 1
;;
      
ls test;;
      
      
      
      
      
                 