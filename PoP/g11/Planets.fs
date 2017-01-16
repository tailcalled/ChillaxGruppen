module Planets
open Geom

type Planet(pos: V3, vel: V3, mass : float, name : string) = class
   
   member this.Pos = pos
   member this.Vel = vel
   member this.Mass = mass
   member this.Name = name

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

let posPlanet (p1 : Planet) (p2 : Planet) (dt : float) =
   let G = 6.67384E-11
   let r = p2.Pos - p1.Pos
   let acc = r * (-(G*p2.Mass) / (r.Norm**2.0))
   let vel = acc * dt
   //let pos = vel * dt
   //Planet(pos, vel, p1.Mass, p1.Name)
   ()

//printfn "%A" (readPlanet "Earth")