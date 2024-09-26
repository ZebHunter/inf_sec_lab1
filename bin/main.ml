
let () = 
  Printf.printf "key: %s\n" (List.fold_left (fun acc x -> acc ^ x) "" Lab1.Polibiy.create_key);
  print_endline "Введите имя файла:";
  let filename = read_line () in 
  let content = Lab1.Polibiy.read_file filename in
  content 
  |> Lab1.Polibiy.encode_text Lab1.Polibiy.create_key 
  |> Lab1.Polibiy.write_text "out.txt";
  print_endline "\n...Encoded...";
  Printf.printf "\n%s\n" (Lab1.Polibiy.read_file "out.txt");
  let content = Lab1.Polibiy.read_file "out.txt" in
  content 
  |> Lab1.Polibiy.decode_text Lab1.Polibiy.create_key 
  |> Lab1.Polibiy.write_text "decoded.txt";
  print_endline "\n...Decoded";
  Printf.printf "\n%s\n" (Lab1.Polibiy.read_file "decoded.txt");

  