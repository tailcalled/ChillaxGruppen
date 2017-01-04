// ================== //
// 4g1 HR 4.11.1 p 89 //
// ================== //

// We have written the following tests:

let testCountFunction (countF : int list * int -> int) =
   let test inList inInt expected =
      let result = countF (inList, inInt)
      if result = expected then
         printfn "[ OK ] count %A %i = %i" inList inInt expected
      else
         printfn "[FAIL] count %A %i = %i != %i" inList inInt result expected
   test [1; 2; 3] 2 1
   test [1; 2; 2; 3] 2 2
   test [] 5 0

// Our first implementation of the function is the following:

let countVirgin (l : int list, x : int) =
   let mutable s = 0
   for i = 0 to l.Length - 1 do
      if l.[i] = x then
         s <- s + 1
   s

printfn "Testing 'countVirgin'"
testCountFunction countVirgin

// Problem: looking up in a list takes O(n), so the above algorithm takes O(n^2)
// time.

let countFast (l : int list, x : int) =
   let mutable s = 0
   let mutable ith = l
   for i = 0 to l.Length - 1 do
      if ith.Head = x then
         s <- s + 1
      ith <- ith.Tail
   s

printfn "Testing 'countFast'"
testCountFunction countFast

// Problem: the above is ugly and hard to understand.

let rec countRec (l : int list, y : int) =
   match l with
    | []                 -> 0
    | x :: xs when x = y -> 1 + countRec (xs, y)
    | x :: xs            -> countRec (xs, y)

printfn "Testing 'countRec'"
testCountFunction countRec

// Problem: the above does not exploit the weakly ascending nature of the list.

/// <summary>
///  Count the number of occurences of y in the weakly ascending list l.
/// </summary>
/// <remarks>
///   The behavior of this function is undefined when l is not weakly ascending.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%i" (count [1; 2; 2; 2; 4] 2)
///   </code>
///   prints "3" to the console.
/// </example>
/// <param name="l">The list in which to count occurences.</param>
/// <param name="y">The element to count occurences of.</param>
/// <returns>The number of occurences of y.</returns>
let rec count (l : int list, y : int) =
   match l with
    | []                 -> 0
    | x :: xs when x > y -> 0
    | x :: xs when x = y -> 1 + count (xs, y)
    | x :: xs            -> count (xs, y)

printfn "Testing 'count'"
testCountFunction count

// ================== //
// 4g1 HR 4.11.2 p 89 //
// ================== //

// We have written the following tests:

let testInsertFunction (insertF : (int list * int) -> int list) =
   let test inList inInt expected =
      let result = insertF (inList, inInt)
      if result = expected then
         printfn "[ OK ] insert %A %i = %A" inList inInt expected
      else
         printfn "[FAIL] insert %A %i = %A != %A" inList inInt result expected
   test [1; 2; 3] 2 [1; 2; 2; 3]
   test [] 4 [4]
   test [-2; 0; 1] -1 [-2; -1; 0; 1]
   test [1; 2] 3 [1; 2; 3]
   test [1; 2] 0 [0; 1; 2]

// Our implementation is the following:

/// <summary>
///  Insert y into the weakly ascending list l, preserving the order of the list
///  and its weakly ascending nature.
/// </summary>
/// <remarks>
///   The behavior of this function is undefined when l is not weakly ascending.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (insert [1; 2; 2; 4] 3)
///   </code>
///   prints "[1; 2; 2; 3; 4]" to the console.
/// </example>
/// <param name="l">The list in to insert an element.</param>
/// <param name="y">The element to insert.</param>
/// <returns>The list with the element inserted.</returns>
let rec insert (l : int list, y : int) =
   match l with
    | []                 -> [y]
    | x :: xs when x < y -> x :: insert (xs, y)
    | x :: xs            -> y :: x :: xs

printfn "Testing 'insert'"
testInsertFunction insert

// ================== //
// 4g1 HR 4.11.3 p 89 //
// ================== //

// We have written the following tests:

let testIntersectFunction (intersectF : int list * int list -> int list) =
   let test list1 list2 expected =
      let result = intersectF (list1, list2)
      if result = expected then
         printfn "[ OK ] intersect %A %A = %A" list1 list2 expected
      else
         printfn "[FAIL] intersect %A %A = %A != %A" list1 list2 result expected
   test [1; 1; 1; 2; 2] [1; 1; 2; 4] [1; 1; 2]
   test [] [] []
   test [] [1; 2; 3] []
   test [1; 2; 3] [] []
   test [1; 2] [3; 4] []
   test [1; 2; 3] [1; 2; 3] [1; 2; 3]

// Our implementation is the following:

/// <summary>
///  Find the intersection of two weakly ascending lists.
/// </summary>
/// <remarks>
///   The behavior of this function is undefined when the lists are not weakly
///   ascending.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (intersect [1; 1; 1; 2; 2] [1; 1; 2; 4])
///   </code>
///   prints "[1; 1; 2]" to the console.
/// </example>
/// <param name="l1">One of the lists to intersect.</param>
/// <param name="l2">The other of the lists to intersect.</param>
/// <returns>The intersection of the two lists.</returns>
let rec intersect (l1, l2) =
   match (l1, l2) with
    | ([], _) | (_, [])             -> []
    | (x :: xs, y :: ys) when x < y -> intersect (xs, (y :: ys))
    | (x :: xs, y :: ys) when x > y -> intersect ((x :: xs), ys)
    | (x :: xs, y :: ys)            -> x :: intersect (xs, ys)

printfn "Testing 'intersect'"
testIntersectFunction intersect

// ================== //
// 4g1 HR 4.11.4 p 89 //
// ================== //

// We have written the following tests:

let testPlusFunction (plusF : (int list * int list) -> int list) =
   let test list1 list2 expected =
      let result = plusF (list1, list2)
      if result = expected then
         printfn "[ OK ] plus %A %A = %A" list1 list2 expected
      else
         printfn "[FAIL] plus %A %A = %A != %A" list1 list2 result expected
   test [1; 1; 2] [1; 2; 4] [1; 1; 1; 2; 2; 4]
   test [] [1; 2] [1; 2]
   test [1; 2] [] [1; 2]
   test [] [] []

// Our implementation is the following:

/// <summary>
///  Find the union of two weakly ascending lists.
/// </summary>
/// <remarks>
///   The behavior of this function is undefined when the lists are not weakly
///   ascending.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (plus [1; 1; 2] [1; 2; 4])
///   </code>
///   prints "[1; 1; 1; 2; 2; 4]" to the console.
/// </example>
/// <param name="l1">One of the lists to union.</param>
/// <param name="l2">The other of the lists to union.</param>
/// <returns>The union of the two lists.</returns>
let rec plus (l1, l2) =
   match (l1, l2) with
    | ([], xs) -> xs
    | (xs, []) -> xs
    | (x :: xs, y :: ys) when x > y -> y :: plus ((x :: xs), ys)
    | (x :: xs, y :: ys)            -> x :: plus (xs, (y :: ys))

printfn "Testing 'plus'"
testPlusFunction plus

// ================== //
// 4g1 HR 4.11.5 p 89 //
// ================== //

// We have written the following tests:

let testMinusFunction (minusF : (int list * int list) -> int list) =
   let test list1 list2 expected =
      let result = minusF (list1, list2)
      if result = expected then
         printfn "[ OK ] minus %A %A = %A" list1 list2 expected
      else
         printfn "[FAIL] minus %A %A = %A != %A" list1 list2 result expected
   test [1; 1; 1; 2; 2] [1; 1; 2; 3] [1; 2]
   test [1; 1; 2; 3] [1; 1; 1; 2; 2] [3]
   test [] [1; 2; 3] []
   test [1; 2; 3] [] [1; 2; 3]
   test [] [] []
   test [1; 2] [1; 2] []

// Our implementation is the following:

/// <summary>
///  Find the difference of two weakly ascending lists.
/// </summary>
/// <remarks>
///   The behavior of this function is undefined when the lists are not weakly
///   ascending.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (minus [1; 1; 1; 2; 2] [1; 1; 2; 3])
///   </code>
///   prints "[1; 2]" to the console.
/// </example>
/// <param name="l1">The list to subtract from.</param>
/// <param name="l2">The list to subtract.</param>
/// <returns>The difference between the two lists.</returns>
let rec minus (l1, l2) =
   match (l1, l2) with
    | ([], xs) -> []
    | (xs, []) -> xs
    | (x :: xs, y :: ys) when x < y -> x :: minus (xs, (y :: ys))
    | (x :: xs, y :: ys) when x > y -> minus ((x :: xs), ys)
    | (x :: xs, y :: ys)            -> minus (xs, ys)

printfn "Testing 'minus'"
testMinusFunction minus

// ================== //
// 4g1 HR 4.15   p 89 //
// ================== //

// We have written the following tests:
let testRevRevFunction (revrevF : int list list -> int list list) =
   let test input expected =
      let result = revrevF input
      if result = expected then
         printfn "[ OK ] revrev %A = %A" input expected
      else
         printfn "[FAIL] revrev %A = %A != %A" input result expected
   test [[1; 2]; [3; 4; 5]] [[5; 4; 3]; [2; 1]]
   test [] []
   test [[]] [[]]
   test [[1]] [[1]]
   test [[]; [1]] [[1]; []]
   test [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] [[9; 8; 7]; [6; 5; 4]; [3; 2; 1]]

// This is our first attempt at an implementation:

let rec revrevBad (l : int list list) =
   let rec reverse (m : int list) =
      match m with
         | [] -> []
         | x :: xs -> reverse xs @ [x]
   match l with
      | [] -> []
      | xs :: xss -> revrevBad xss @ [reverse xs]

printfn "Testing 'revrevBad'"
testRevRevFunction revrevBad

// Problem: appending lists takes O(n), so our reverse function above takes
// O(n^2), which makes the revrevBad function take O(m^2 + m n^2), where
// n is the average length of the inner lists and m is the number of inner
// lists.

/// <summary>
///  Reverses a list of lists and all of its inner lists.
/// </summary>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (revrev [[1; 2]; [3; 4; 5]])
///   </code>
///   prints "[[5; 4; 3]; [2; 1]]" to the console.
/// </example>
/// <param name="l">The list to revrev.</param>
/// <returns>The revrev of the list.</returns>
let revrev (l : int list list) =
   let rec reverseAux (m : int list) (acc : int list) =
      match m with
       | [] -> acc
       | x :: xs -> reverseAux xs (x :: acc)
   let reverse m = reverseAux m []
   let rec revrevAux (m : int list list) (acc : int list list) =
      match m with
       | [] -> acc
       | xs :: xss -> revrevAux xss (reverse xs :: acc)
   revrevAux l []

printfn "Testing 'revrev'"
testRevRevFunction revrev

// ================== //
// 4g2                //
// ================== //

// We have written the following tests:
let testRemDupesFunction (remDupes : int list -> int list) =
   let test input expected =
      let result = remDupes input
      if result = expected then
         printfn "[ OK ] removeDuplicates %A = %A" input expected
      else
         printfn "[FAIL] removeDuplicates %A = %A != %A" input result expected
   test [1; 2; 1; 3; 2] [1; 2; 3]
   test [] []
   test [1; 2; 3] [1; 2; 3]
   test [1; 1; 1] [1]

// Our implementation is the following:

/// <summary>
///  Remove the duplicates in the list.
/// </summary>
/// <remarks>
///   This function preserves the order of the elements, and never removes the
///   first copy of duplicates.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (removeDuplicates [1; 2; 1; 3; 2])
///   </code>
///   prints "[1; 2; 3]" to the console.
/// </example>
/// <param name="l">The list to remove duplicates from.</param>
/// <returns>The list without any duplicates.</returns>
let rec removeDuplicates (l : 'a list when 'a : equality) =
   let rec remove x m =
      match m with
         | []                 -> []
         | y :: ys when y = x -> remove x ys
         | y :: ys            -> y :: remove x ys
   match l with
    | [] -> []
    | x :: xs -> x :: removeDuplicates (remove x xs)

printfn "Testing 'removeDuplicates'"
testRemDupesFunction removeDuplicates
