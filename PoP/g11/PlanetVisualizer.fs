module PlanetVisualizer
open Geom
open Planets
open System.Timers
open System.Drawing
open System.Windows.Forms

type PlanetsForm () as this = class
   inherit Form ()

   do
      this.DoubleBuffered <- true
      this.FormBorderStyle <- FormBorderStyle.None
      this.WindowState <- FormWindowState.Maximized
end

type ZoomMode = ZoomClose | ZoomFar | ZoomAuto

let mutable zoomModes = [ZoomAuto; ZoomClose; ZoomFar]
let mutable speed = 1.0

let mutable solarSystem = new SolarSystem(0.0)
let mutable time = solarSystem.T0
let form = new PlanetsForm()
let mutable dt = 0.0
let mutable dtSmooth = 0.0
let font = new Font("Arial", 16.0F)

let paint (gfx : Graphics) =
   let width = form.ClientSize.Width
   let height = form.ClientSize.Height
   let zoom =
      match zoomModes.Head with
      | ZoomClose -> 200.0F
      | ZoomFar -> 20.0F
      | ZoomAuto ->
         solarSystem.Time time |> List.map (fun p ->
               0.5F * float32 height / (float32 p.Pos.Norm + 1.0F)
            ) |> List.min
   gfx.FillRectangle (Brushes.Black, 0, 0, width, height)
   let fps = 1.0 / dtSmooth
   let daysSince1962Jan20 = time - 2439126.5
   let daysSince1962Jan1 = daysSince1962Jan20 - 19.0
   let years = int (daysSince1962Jan1 / 365.0)
   let year = years + 1962
   let daysSinceYear = daysSince1962Jan1 - float years * 365.0
   let months =
      ["Jan"; "Feb"; "Mar"; "Apr";
       "May"; "June"; "July"; "Aug";
       "Sep"; "Oct"; "Nov"; "Dec"]
   let month = months.[int (daysSinceYear * 12.0 / 365.0)]
   let lines =
      [
         sprintf "FPS: %A" fps;
         sprintf "%s%A" month year;
         sprintf "Zoom: %A" zoomModes.Head;
         sprintf "Speed: %A days/second" (speed - 1.0);
         "";
         "Press [Z] to";
         "change zoom";
         "mode and [R] to";
         "reload JPL";
         "Ephemeris data."
         "Up/down arrows";
         "change speed."
      ]
   for (i, line) in List.zip [0 .. lines.Length - 1] lines do
      gfx.DrawString(line, font, Brushes.White, 0.0F, 20.0F * float32 i)
   let rand = new System.Random(5918421)
   for i = 0 to 5000 do
      let x = rand.Next(width)
      let y = rand.Next(height)
      gfx.FillRectangle (Brushes.White, x, y, 1, 1)
   gfx.TranslateTransform(float32 width / 2.0F, float32 height / 2.0F)
   for planet in solarSystem.Time time do
      let x = float32 planet.Pos.X * zoom
      let y = float32 planet.Pos.Y * zoom
      let sz = log (float32 planet.Mass * 2.714F / 1.3E22F) / 6.0F + 2.0F
      let b = if planet.Name = "Sol" then Brushes.Yellow else Brushes.Gray
      gfx.FillEllipse(b, x - sz, y - sz, 2.0F * sz, 2.0F * sz)
      gfx.DrawString (planet.Name, font, Brushes.White, x, y)

let runSimulation () =
   if 1.0 / dt < 10.0 then
      speed <- 1.0
   else
      time <- time + dt * (speed - 1.0)

form.Paint.Add (fun e ->
   paint e.Graphics
)
form.KeyDown.Add (fun e ->
   match e.KeyCode with
   | Keys.Z -> zoomModes <- zoomModes.Tail @ [zoomModes.Head]
   | Keys.R -> solarSystem <- new SolarSystem(time)
   | Keys.Up -> speed <- speed * 2.0
   | Keys.Down ->
      if speed > 1.0 then speed <- speed / 2.0
   | _ -> ()
)
let clock = new System.Timers.Timer(1000.0 / 120.0)
let mutable lastTime = 0.0
clock.Elapsed.Add (fun e ->
   let nowTime = float e.SignalTime.Ticks / 10000000.0
   if lastTime <> 0.0 then
      dt <- nowTime - lastTime
      dtSmooth <- dtSmooth * 0.99 + dt * 0.01
      runSimulation ()
      form.Refresh ()
   lastTime <- nowTime
)
clock.Start()
Application.Run form