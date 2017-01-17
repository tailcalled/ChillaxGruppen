module Util

let rec splitWhile p = function
   | [] -> ([], [])
   | x :: xs when p x ->
      let (bs, cs) = splitWhile p xs
      (x :: bs, cs)
   | x :: xs -> ([], x :: xs)
let dropWhile p = splitWhile p >> snd