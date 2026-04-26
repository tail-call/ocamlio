let dictionary =
  object
    val hash = Hashtbl.create 100
    val mutable last_index = 0
    method maybe_add word =
      (* Is word already in hash? *)
      if Hashtbl.mem hash word then
        false
      (* If not, add it *)
      else begin
        Hashtbl.add hash word last_index;
        last_index <- last_index + 1;
        true
      end
    method print_contents =
      Hashtbl.iter (fun k v -> Printf.printf "%s: %d\n" k v) hash
    method tokenize words =
      List.map (fun w ->
        if Hashtbl.mem hash w then
          Hashtbl.find hash w
        else
          -1
      ) words 
  end

let words_of sentence =
  String.split_on_char ' ' sentence 

let test_dict =
  let sentence = "a cat sat on the mat what a beautiful cat" in 
  let words = words_of sentence in
  let dict = dictionary in begin
    List.iter (fun x -> begin ignore (dict#maybe_add x); () end) words;
    dict
  end

let test1 () = test_dict#print_contents

let test2 () = test_dict#tokenize (words_of "what a beautiful day and a cat")

let neighbor_indices_of ~i ~window ~last_index =
  let ceiling = last_index - 1 in
  let start = max 0 (i - window) in
  let stop = min ceiling (i + window) in
  let rec loop acc current =
    if current < start then
      acc
    else if current == i then
      loop acc (current - 1)
    else
      loop (current :: acc) (current - 1)
  in
  if start > stop then [] else loop [] stop

let generate_pairs items window =
  List.flatten (
    List.mapi (fun i x ->
      let ranges = neighbor_indices_of
        ~i
        ~window:2
        ~last_index:(List.length items)
      in
      List.map (fun j -> (List.nth items j, x)) ranges
    ) items
  )

let test3 () =
  let data = test2 () in
    generate_pairs data 2

;;

test3()