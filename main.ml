(* Import the required modules *)
open Stdlib
open Format


(* Variables *)
let number = 20

(* Functions *)
let inc (x: int): int = x + 1
let result = inc number
let result_is_greater (result: int): bool = result > number
let final_result = if result_is_greater result then result else number

(* Recursive Function *)
let rec fact (n: int): int  = if n = 0 then 1 else n * fact (n-1)

let fib =
  let rec aux (a: int) (b: int) (n: int):int = 
    if n <= 1 then b else aux b (a + b) (n-1)
  in aux 0 1

let fib_result = fib final_result

let sumrange =
  let rec aux (acc: int) (a: int) (b: int):int = 
    if a > b then acc else aux (a + acc) (a + 1) b
  in aux 0

let final_result_fact = fact final_result
let sumrange_result = sumrange number final_result


(* Lambda Function *)
let relu = (fun x -> if x <= 0 then 0 else x)
let relu_result = relu final_result

(* Currying *)
let sum = (fun a -> (fun b -> (fun c -> a + b + c)))
let sum_result = sum number final_result relu_result

(* Printing *)
let () = printf "Final result: %d\n" final_result
let () = printf "Factorial: %d\n" final_result_fact
let () = printf "ReLU: %d\n" relu_result
let () = printf "Sum: %d\n" sum_result
let () = printf "Sum Range: %d\n" sumrange_result
let () = printf "Fibonacci: %d\n" fib_result
let () = printf "what's your name again? "

let () = 
  let name = read_line () in printf "Hello %s\n" name;