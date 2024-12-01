let input = "./input.txt"

let () = 
    let input_channel = open_in input in
    try
        let rec read_lines list_1 list_2 =
            try
                let line = input_line input_channel in
                let parts = String.split_on_char ' ' line in
                    match parts with
                | first :: _ :: _ :: last :: [] -> read_lines (int_of_string first :: list_1) (int_of_string last :: list_2) 
                    | _ -> failwith "Invalid input format"
            with End_of_file -> (list_2,  list_1)
        in
        let (list_1, list_2) = read_lines [] [] in
        let sorted_list_1 = List.sort compare list_1 in
        let sorted_list_2 = List.sort compare list_2 in
        let distances = List.map2 (fun x y -> abs (x-y)) sorted_list_1 sorted_list_2 in
        let total_distance = List.fold_left (+) 0 distances in
        Printf.printf "%d\n" total_distance;
        close_in input_channel;
    with error ->
        close_in_noerr input_channel;
        raise error
