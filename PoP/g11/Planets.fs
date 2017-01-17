module Planets
open Geom
open Util

type Planet(pos: V3, vel: V3, mass : float, name : string) = class
   
   member this.Pos = pos
   member this.Vel = vel
   member this.Mass = mass
   member this.Name = name

   override this.ToString () = sprintf "%s %A" name mass

end

let readPlanet planetName =
   use stream = System.IO.File.OpenText ("data/" + planetName + ".txt")
   let readNumber text =
      let start = text |> dropWhile ((=) ' ')
      let isNumeric c = (c >= '0' && c <= '9') || c = '.' || c = '-'
      let (num, rest) = start |> splitWhile isNumeric
      (num |> List.map string |> String.concat "" |> float, rest)
   let rec findMass () =
      let line = stream.ReadLine()
      if line.Contains "Mass" then
         let line1 = line.Substring (line.IndexOf "Mass")
         let line2 = line1.Substring (line1.IndexOf "=" + 1)
         let line3 = line1.Substring (line1.IndexOf "^" + 1)
         let (mass, _) = readNumber (List.ofSeq line2)
         let (magnitude, _) = readNumber (List.ofSeq line3)
         mass * (10.0 ** magnitude)
      else findMass ()
   let mass = findMass ()
   while stream.ReadLine() <> "$$SOE" do
      ()
   let readEntry line =
      let (time, line1) = readNumber line
      let (lon, line2) = readNumber line1
      let (lat, line3) = readNumber line2
      let (rad, line4) = readNumber line3
      let (rdot, _) = readNumber line4
      (time, lon, lat, rad, rdot)
   let rec readEntries acc =
      let mutable line = stream.ReadLine ()
      if line = "$$EOE" then List.rev acc
      else
         let entry = readEntry (List.ofSeq line)
         readEntries (entry :: acc)
   (planetName, mass, readEntries [])

let planetNames =
   ["Earth"; "Jupiter"; "Mars"; "Mercury";
    "Neptune"; "Pluto"; "Saturn"; "Uranus"; "Venus"]

let entryTime (time, lon, lat, rad, rdot) = time
let sinD x = sin (x / 180.0 * 3.1415)
let cosD x = cos (x / 180.0 * 3.1415)
let computePosition (time, l, b, r, rdot) =
   V3(r * cosD b * cosD l, r * cosD b * sinD l, r * sinD b)

let constructPlanets planets =
   let rec findTime times =
      let maxTime = times |> List.map List.head |> List.max
      if times |> List.forall (fun ts -> ts.[0] = maxTime) then
         maxTime
      else
         findTime (times |> List.map (dropWhile ((>) maxTime)))
   let firstTime =
      findTime (
         planets |> List.map (
            fun (name, mass, entries) -> List.map entryTime entries
         ) |> List.tail
      ) + 1.0
   let planetEntries = planets |> List.map (fun (name, mass, entries) ->
         let (a, b) = entries |> splitWhile (entryTime >> (>) firstTime)
         (name, mass, List.last a, b)
      )
   let constructPlanet (name, mass, e0, e1 :: e2 :: _) =
      let p0 = computePosition e0
      let p1 = computePosition e1
      let p2 = computePosition e2
      let deltaT = entryTime e2 - entryTime e0
      let v = (p2 - p0) / deltaT
      Planet(p1, v, mass, name)
   (firstTime, planetEntries |> List.map constructPlanet)

let planetData = planetNames |> List.map readPlanet
let (t0, planets) = constructPlanets planetData
let sol = new Planet(V3(0.0, 0.0, 0.0), V3(0.0, 0.0, 0.0), 1.989E30, "Sol")

/// <param name="dt">Delta T in days</param>
let simulate dt (system : Planet list) =
   system |> List.map (fun planet ->
      let (acc: V3, jerk: V3) =
         system |> List.map (fun other ->
            if planet = other then
               (V3(0.0, 0.0, 0.0), V3(0.0, 0.0, 0.0))
            else
               let M = other.Mass
               let r = planet.Pos - other.Pos
               let dr = planet.Vel - other.Vel
               let rN = r.Norm
               let dRN = dr .* r / rN
               let rN3 = rN * rN * rN
               let drN3 = 3.0 * rN * rN * dRN
               let G = 1.488E-34
               let ratio = - G * M/rN3
               let dratio = (G * M * drN3) / (rN3 * rN3)
               let a = ratio * r
               let j = dratio * r + ratio * dr
               (a, j)
         ) |> List.reduce (fun (xa, xj) (ya, yj) -> (xa + ya, xj + yj))
      let newPos = planet.Pos + dt * planet.Vel + 0.5 * dt * dt * acc
      let newVel = planet.Vel + dt * acc + 0.5 * dt * dt * jerk
      new Planet(newPos, newVel, planet.Mass, planet.Name)
   )

type SolarSystem() = class
   
   let initSystem = sol :: planets
   let initTime = t0

   let simulateSeq dt =
      let mutable now = initSystem
      Seq.cache (seq {
         while true do
            for i = 1 to 1000 do
               now <- simulate (dt / 1000.0) now
            yield now
      })

   let future = simulateSeq 1.0
   let past = simulateSeq -1.0

   let day n =
      let m = n - int initTime
      if m < 0 then Seq.item (-m - 1) past
      else if m > 0 then Seq.item (m - 1) future
      else initSystem
   member this.Time fakeT =
      let t = fakeT - 0.5
      let date = int t
      let frac = t - float date
      let ifrac = 1.0 - frac
      let p0 = day date
      let p1 = day (date + 1)
      List.zip p0 p1 |> List.map (fun (pl0, pl1) ->
            let pos = pl0.Pos * ifrac + pl1.Pos * frac
            let vel = pl0.Vel * ifrac + pl1.Vel * frac
            new Planet(pos, vel, pl0.Mass, pl0.Name)
         )

end