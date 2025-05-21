exception MemOverFlow of string
exception UnfinishedLoop

let rec init_arr = function
  | 0 -> []
  | n -> 0 :: init_arr (n - 1)

let rec print_memory = function
  | [] -> print_endline "[]"
  | h :: [] -> print_string "["; print_int h; print_endline "]"
  | h :: t -> print_string "["; print_int h; print_string "] "; print_memory t

let explode strdata =
  List.of_seq (String.to_seq strdata)

let getSquaredList data =
  let rec aux l acc count = match l with
    | [] -> raise UnfinishedLoop
    | ']' :: t when count = 1 -> (List.rev acc), t
    | ']' :: t -> aux t (']' :: acc) (count - 1)
    | '[' :: t -> aux t ('[' :: acc) (count + 1)
    | h :: t -> aux t (h :: acc) count

  in if List.hd data != '[' then raise Not_found
  else aux (List.tl data) [] 1

let interpret code_data input_data = 
  let mem_limit = 15 in
  let memory = init_arr mem_limit in
  
  (*Note: prev_mem is reversed*)
  let rec aux code input output prev_mem curr_mem next_mem = 
    (*print_memory prev_mem; print_string ("* " ^ (Int.to_string curr_mem) ^ " *\n"); print_memory next_mem; print_newline ();*)
    match code with
      | '.' :: t -> aux t input (Char.chr curr_mem :: output) prev_mem curr_mem next_mem
      | ',' :: t -> (
        match input with
          | [] -> raise Not_found
          | hchar :: tinput -> aux t tinput output prev_mem (Char.code hchar) next_mem
      )

      | '+' :: t -> aux t input output prev_mem (curr_mem + 1) next_mem
      | '-' :: t -> 
        if curr_mem = 0 then raise (MemOverFlow "Cannot Decrement Below Zero.")
        else aux t input output prev_mem (curr_mem - 1) next_mem

      | '>' :: t -> 
        if next_mem = [] then raise (MemOverFlow ("Cannot Move Forward Past " ^ (Int.to_string mem_limit) ^ "."))
        else aux t input output (curr_mem :: prev_mem) (List.hd next_mem) (List.tl next_mem)

      | '<' :: t ->
        if prev_mem = [] then raise (MemOverFlow "Cannot Move Backward Past Zero.")
        else aux t input output (List.tl prev_mem) (List.hd prev_mem) (curr_mem :: next_mem)

      | '[' :: _ -> 
        let loopcode, nextcode = getSquaredList code in
        let p, c, n, o = loop loopcode input output prev_mem curr_mem next_mem in
        aux nextcode input o p c n

      | '$' :: t -> print_memory (List.rev prev_mem @ [curr_mem] @ next_mem); aux t input output prev_mem curr_mem next_mem
      | _ :: t -> aux t input output prev_mem curr_mem next_mem
      | [] -> prev_mem, curr_mem, next_mem, output

  and loop code input output prev_mem curr_mem next_mem =
    if curr_mem = 0 then prev_mem, curr_mem, next_mem, output
    else let p, c, n, o = aux code input output prev_mem curr_mem next_mem
    in loop code input o p c n
  
  in let _, _, _, o = aux (explode code_data) input_data [] [] (List.hd memory) (List.tl memory) in
  String.of_seq (List.to_seq (List.rev o))

let () = print_string (interpret ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]
>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++
.------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++." [])

let () = print_endline (interpret "+++++[>++>++<<-] // 0 @ pos 0; 10 @ pos 1 and 2
>[>[>+>+<<-]>>[<<+>>-]<<<-] // 0 @ pos 0; 0 @ pos 1; 10 @ pos 2; 100 @ pos 3; 0 @ pos 4

>>> // move cursor to pos 4

// To print n
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ [>+<-]< // 0 @ pos 4; n @ pos 5

[
>>[>]+>+[<]<-[>>[>]<+<+[<]<-]>>[>]<[[<]<+>>[>]<-]< // power @ pos 6
<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]< // copied pos 5 into pos 8
<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]< // copied pos 6 into pos 9
>+>[-]>[-]>[-]<<<<

[
    - // decrement pos 9
    [>[-]] // if pos 9 is non zero; go to pos 10; make it zero
    > // if pos 9 was non zero; going to pos 11 (zero) else pos 10 (which is nonzero if pos 9 was zero)
    
    [[-]<<<+<[<->>>>+>+<<<<-]>>>>[<<<<+>>>>-]>] // runs only if pos 9 was zero; since then it goes to pos 10 (which is nonzero cuz pos 9 was zero)
    // makes 10 zero; goes to pos 7 and increments it
    // copies pos 6 value into pos 9 using pos 10; and simultaneously reduces pos 5 by pos 6
    // finally goes to pos 11
    
    <+< // now at pos 11 (both cases); sets pos 10 (zero in both cases) to 1; comes back to pos 9
    
    <- // decrement pos 8
    
    [>>[-]] // if pos 8 nonzero; go to pos 10 and make it zero
    >> // if pos 8 nonzero; go to pos 12 (zero) else pos 10 (nonzero since pos 8 nonzero)
    [<[-]>[-]>>] // runs only if pos 8 was nonzero; since then only it is at pos 10 (nonzero cuz pos 8 is nonzero)
    // makes pos 9 and 10 zero and reaches pos 12 (zero)

    <<+< // now at pos 12 (both cases); inc pos 10 to 1 and go to pos 9
]
>[-]<<[-]< // zero in pos 8 and 10 and go to pos 7
++++++++++++++++++++++++++++++++++++++++++++++++.[-] // add 48 and print and then set to 0
<[-]<<[<]+<[-]<
]

// at pos 1
>>[-]>[-]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]" [])