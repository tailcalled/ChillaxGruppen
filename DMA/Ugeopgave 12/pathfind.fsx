type Graph = {
      size : int;
      edges : Set<int * int>;
      weight : (int * int -> float)
   }

type Search = float * int

let setToMap n s =
   [0 .. n] |> List.map (fun k ->
      (k, s |> Set.filter (fun (a, _) -> a = k) |> Set.map snd)
   ) |> Map.ofList

let pathfind (g : Graph) (a : int) (b : int) =
   let neighbours = setToMap g.size g.edges
   let mutable queue = Set.ofList[(0.0, a)]
   let dists = Array.init g.size (fun _ -> infinity)
   while not queue.IsEmpty do
      let (dist, node) = queue.MinimumElement
      queue <- queue.Remove (dist, node)
      if dists.[node] > dist then
         dists.[node] <- dist
         for neighbour in Set.toSeq neighbours.[node] do
            let cost = - log(g.weight(node, neighbour))
            queue <- queue.Add (dist + cost, neighbour)
   if dists.[b] = infinity then
      None
   else
      let reversed =
         g.edges |> Set.map (fun (x, y) -> (y, x)) |> setToMap g.size
      let mutable path = [b]
      while path.Head <> a do
         let neighs = reversed.[path.Head] |> Set.toList
         let prev = neighs |> List.minBy (fun node -> dists.[node])
         path <- prev :: path
      Some path