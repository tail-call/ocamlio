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

;;

test2 ()
