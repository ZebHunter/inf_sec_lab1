(* 
  Служебная функция, предоставляющая инфиксный оператор 
  для записи диапазонов широких кодировок 

  val ( -- ): Uchar.t -> Uchar.t -> Uchar.t list = <fun>
*)

let rec ( -- ) a b = 
  match compare a b with
  | 1 -> []
  | _ -> a :: (Uchar.succ a -- b)

(*
  Функция для преобразования кодировки в строку

  val decode_uchar: Uchar.t -> string = <fun>
*)

let decode_uchar u = let b = Buffer.create 4 in 
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

(*
  Преобразование списка Uchar.t к строковому типу
  Данная функция необходима для обеспечения целосотности работы с русским алфавитом

  val map_uchar_to_string: Uchar.t list -> string list = <fun>
*)

let map_uchar_to_string l = List.map (fun x -> decode_uchar x) l

(*
  Функция перемешивания списка
  Нужна для обеспечения рандомности квадрата Полибия

  val shuffle: a' list -> a' list = <fun>
*)

let shuffle list =
  let shuffled = List.map (fun x -> (Random.bits(), x)) list in
  let sorted = List.sort compare shuffled in
  List.map snd sorted

(*
  Функция для обеспечения большей степени рандомизации квадрата,
  т.к. элементы квадрата статичны

  val shuffle_random_times: int -> a' list -> a' list = <fun>
*)

let shuffle_random_times n list =
  let reshuffle_count = Random.int n + 1 in
  let rec apply_shuffle n lst =
    if n <= 0 then lst
    else apply_shuffle (n - 1) (shuffle lst)
  in
  apply_shuffle reshuffle_count list

(*
  Функция получения кода символа из квадрата Полибия

  val polibiy_index: a' -> a' list -> int * int = <fun>
*)

let polibiy_index e l = 
  let i = List.find_index (fun x -> x = e) l in
  match i with
  | Some i -> (i / 9 , i mod 9)
  | None -> (-1, -1)

(*
  Функция получения символа из квадрата Полибия по индексу

  val el_by_index: int * int -> a' list -> a' = <fun>
*)

let el_by_index (a, b) l = 
  List.nth l (a * 9 + b)

(*
  Функция записи в файл

  val write_text: string -> string -> unit = <fun>
*)

let write_text filename ul =
  let oc = open_out filename in
  output_string oc ul;
  close_out oc

(*
  Функция чтения из файла

  val read_file: string -> string = <fun>
*)

let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  String.concat "\n" (read_lines [])

(*
  Функция перевода списка char к строке

  val chars_to_string: char list -> string = <fun>
*)

let chars_to_string cl = let b = Buffer.create 16 in 
  List.iter (Buffer.add_char b) cl;
  Buffer.contents b 

(*
  Функция кодирования текста с рандомным квадратом Полибия

  val encode_text: string list -> string -> string = <fun>
*)

let encode_text key str =
  let s = List.init (String.length str) (String.get str) in
  let rec encode s key acc = 
    match s with
    | [] -> List.rev acc
    | x1 :: x2 :: xs when x1 = '\208' || x1 = '\209' -> let (a, b) = polibiy_index (chars_to_string (x1 :: x2 :: [])) key in
                                      encode xs key (b :: a :: acc)
    | h :: t -> let (a, b) = polibiy_index (String.make 1 h) key in
                encode t key (b :: a :: acc)
  in
  encode s key [] |> List.fold_left (fun acc1 x -> acc1 ^ string_of_int x) ""

(*
  Функция декодирования текста с рандомным квадратом Полибия

  val decode_text: string list -> string -> string = <fun>
*)

let decode_text key str = 
  let s = List.init (String.length str) (String.get str) in
  let i = List.map (fun x -> (int_of_char x) - 48) s in
  let rec decode i key acc =
    match i with
    | [] -> List.rev acc
    | x1 :: x2 :: xs -> let el = el_by_index (x1, x2) key in
                        decode xs key (el :: acc)
    | _ :: [] -> raise (Failure "Ошибка декодирования")
  in 
  decode i key [] |> List.fold_left (fun acc1 x -> acc1 ^ x) ""

(*
  Функция создания ключа с параметром от длины текста

  val create_key: int -> string list = <fun>
*)

let create_key n = (Uchar.of_int 1040) -- (Uchar.of_int 1103) 
  |> map_uchar_to_string 
  |> List.append [" "; ";"; "."; ","; "-"; "\n"; "\t"; "?"; "!"; "ё"; ":"; "\""; "("; ")"]
  |> shuffle_random_times n

(*
  Функция чтения квадрата Полибия из файла (нужна для декодирования)

  val read_key: string -> string list = <fun>
*)

let read_key filename =
  let k = read_file filename in
  let key = List.init (String.length k) (String.get k) in
  let rec key_to_list key acc =
    match key with
    | [] -> List.rev acc
    | x1 :: x2 :: xs when x1 = '\208' || x1 = '\209' -> key_to_list xs ( (chars_to_string (x1 :: x2 :: [])) :: acc )
    | h :: t -> key_to_list t ((chars_to_string (h :: [])) :: acc)
  in
  key_to_list key []
