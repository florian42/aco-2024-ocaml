let input = "./input.txt"

let count_occurrences lst =
  let table = Hashtbl.create (List.length lst) in
  List.iter
    (fun item ->
      Hashtbl.replace table item
        (1 + (Hashtbl.find_opt table item |> Option.value ~default:0)))
    lst;
  table

let () =
  let input_channel = open_in input in
  try
    let rec read_lines list_1 list_2 =
      try
        let line = input_line input_channel in
        let parts = String.split_on_char ' ' line in
        match parts with
        | [ first; _; _; last ] ->
            read_lines
              (int_of_string first :: list_1)
              (int_of_string last :: list_2)
        | _ -> failwith "Invalid input format"
      with End_of_file -> (list_2, list_1)
    in
    let similiarity_score =
      ( read_lines [] [] |> fun (list_1, list_2) ->
        (list_1, count_occurrences list_2) )
      |> fun (list_1, occurrences) ->
      List.map
        (fun x ->
          (Hashtbl.find_opt occurrences x |> Option.value ~default:0) * x)
        list_1
      |> List.fold_left ( + ) 0
    in
    Printf.printf "%d\n" similiarity_score;
    close_in input_channel
  with error ->
    close_in_noerr input_channel;
    raise error
