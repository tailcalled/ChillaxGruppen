module Planets
open Geom

type Planet(pos: V3, vel: V3) = class
   
   member this.Pos = pos
   member this.Vel = vel

end

let readPlanet planetName =
   use stream = System.IO.File.OpenText ("data/" + planetName + ".txt")
   while stream.ReadLine() <> "$$SOE" do
      ()
   let rec splitWhile p = function
      | [] -> ([], [])
      | x :: xs when p x ->
         let (bs, cs) = splitWhile p xs
         (x :: bs, cs)
      | x :: xs -> ([], x :: xs)
   let dropWhile p = splitWhile p >> snd
   let readNumber text =
      let start = text |> dropWhile ((=) ' ')
      let (num, rest) = start |> splitWhile ((<>) ' ')
      (num |> List.map string |> String.concat "" |> float, rest)
   let readEntry line =
      let (time, line1) = readNumber line
      let (lon, line2) = readNumber line1
      let (lat, line3) = readNumber line2
      let (rad, line4) = readNumber line3
      let (rdot, _) = readNumber line4
      (time, lon, lat, rad, rdot)
   let rec readEntries () =
      let mutable line = stream.ReadLine ()
      if line = "$$EOE" then []
      else
         let entry = readEntry (List.ofSeq line)
         entry :: readEntries ()
   readEntries ()

printfn "%A" (readPlanet "Earth")