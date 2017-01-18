module JPLECompare
open Geom
open Planets
open Util

let system = new SolarSystem(0.0)

let dataMap =
   let mutable acc = Map.empty
   for (name, _, entries) in planetData do
      for entry in entries do
         let time = entryTime entry
         let pos = computePosition entry
         let planetmap =
            match Map.tryFind time acc with
            | Some x -> x
            | None -> Map.empty
         let newmap = Map.add name pos planetmap
         acc <- Map.add time newmap acc
   acc

printf "Time"
for planet in system.Time system.T0 do
   if planet.Name <> "Sol" then
      printf ",%s" planet.Name
printfn ""
for (time, positions) in Map.toSeq dataMap do
   let state = system.Time time
   printf "%A" time
   let sol = state |> List.find (fun p -> p.Name = "Sol")
   for planet in state do
      if planet.Name <> "Sol" then
         match Map.tryFind planet.Name positions with
         | Some pos ->
            let dist = (planet.Pos - sol.Pos - pos).Norm
            printf ",%A" dist
         | None ->
            printf ",JPLE DATA NOT AVAILABLE"
   printfn ""