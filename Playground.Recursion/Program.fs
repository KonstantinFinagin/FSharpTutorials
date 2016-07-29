open System

// sample of a tail recursion
let map converter l = 
    let rec loop accumulator = function
        | [] -> accumulator
        | head :: tail -> loop(converter head :: accumulator) tail 
    List.rev (loop [] l)


let a = map (fun x -> x * x) [1;2;3;4;5]

let result =Console.ReadLine()
0