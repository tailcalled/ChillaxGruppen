(*topSort*)
// first, define the relations on A and A*A
let aRel x y = (y % x = 0)
let aSqrRel (a1, a2) (b1, b2) =
   (a1 <> b1 && aRel a1 b1) || (a1 = b1 && aRel a2 b2)

// define the set to be sorted
let unsorted = Set.ofList [(2, 3); (4, 6); (2, 10); (10, 2); (30, 30); (2, 30)]

// define the strict version of the order relation
let bigger x y = aSqrRel y x && x <> y

// define a sorting algorithm
// NOTE: this is slow (like O(n^3)) and thus not suited for big inputs
let rec topSort set =
   if set = Set.empty then []
   else
      let found =
         set |> Set.toList
             |> List.tryFind (fun x -> not (Set.exists (bigger x) set))
      let elem = Option.get found
      elem :: topSort (Set.remove elem set)

// sort the set
let sorted = topSort unsorted

// output
printfn "%A" sorted

(*end*)