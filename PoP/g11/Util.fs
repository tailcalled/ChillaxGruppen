module Util

let rec splitWhile p = function
   | [] -> ([], [])
   | x :: xs when p x ->
      let (bs, cs) = splitWhile p xs
      (x :: bs, cs)
   | x :: xs -> ([], x :: xs)
let dropWhile p = splitWhile p >> snd

let memo f =
   let mutable cache = Map.empty
   fun x ->
      match Map.tryFind x cache with
      | Some y -> y
      | None ->
         let y = f x
         cache <- Map.add x y cache
         y

let repeat n f x =
   let mutable y = x
   for i = 1 to n do
      y <- f y
   y