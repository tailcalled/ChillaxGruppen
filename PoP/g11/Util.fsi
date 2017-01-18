module Util

/// Split the list in two at the first element where the predicate yields false
val splitWhile : ('a -> bool) -> 'a list -> ('a list * 'a list)
/// Drops elements until the predicate yields true
val dropWhile : ('a -> bool) -> ('a list -> 'a list)

/// Memoizes the results of the input function
val memo : ('a -> 'b) -> ('a -> 'b) when 'a : comparison

/// Repeats the input function a number of times
val repeat : int -> ('a -> 'a) -> 'a -> 'a