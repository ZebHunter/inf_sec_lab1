let rec ( -- ) a b = 
  match compare a b with
  | 1 -> []
  | _ -> a :: (Uchar.succ a -- b)

let decode_uchar u = let b = Buffer.create 4 in 
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let map_uchar_to_string l = List.map (fun x -> decode_uchar x) l

let shuffle list =
  let shuffled = List.map (fun x -> (Random.bits(), x)) list in
  let sorted = List.sort compare shuffled in
  List.map snd sorted

let shuffle_random_times list =
  let reshuffle_count = Random.int 10 + 1 in
  let rec apply_shuffle n lst =
    if n <= 0 then lst
    else apply_shuffle (n - 1) (shuffle lst)
  in
  apply_shuffle reshuffle_count list

let polibiy_index e l = 
  let i = List.find_index (fun x -> x = e) l in
  match i with
  | Some i -> (i / 9 , i mod 9)
  | None -> (-1, -1)

let el_by_index (a, b) l = 
  List.nth l (a * 9 + b)

let write_text filename ul =
  let oc = open_out filename in
  output_string oc ul;
  close_out oc

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

let chars_to_string cl = let b = Buffer.create 16 in 
  List.iter (Buffer.add_char b) cl;
  Buffer.contents b 

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

let create_key = (Uchar.of_int 1040) -- (Uchar.of_int 1103) 
  |> map_uchar_to_string 
  |> List.append [" "; ";"; "."; ","; "-"; "\n"; "\t"; "?"; "!"; "ё"; ":"; "\""; "("; ")"]
  |> shuffle_random_times 
