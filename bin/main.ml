(*
  Точка входа в программу
*)

let () = 
  print_endline "Введите имя файла:";
  let filename = read_line () in 
    let content = Lab1.Polibiy.read_file filename in
      let key = Lab1.Polibiy.create_key (String.length content) in
      let print_key = Lab1.Polibiy.write_text "key.txt" (List.fold_left (fun acc x -> acc ^ x) "" key) in
      print_key;
        content 
        |> Lab1.Polibiy.encode_text key 
        |> Lab1.Polibiy.write_text "out.txt";
  print_endline "\n...Encoded...";
  Printf.printf "\n%s\n" (Lab1.Polibiy.read_file "out.txt");
  print_endline "Введите имя файла для декодирования:";
  let filename = read_line () in 
    let content = Lab1.Polibiy.read_file filename in
    let key = Lab1.Polibiy.read_key "key.txt" in
      content 
      |> Lab1.Polibiy.decode_text key
      |> Lab1.Polibiy.write_text "decoded.txt";
  print_endline "\n...Decoded";
  Printf.printf "\n%s\n" (Lab1.Polibiy.read_file "decoded.txt");

  