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
let mutable seekYear = 0

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
   let years = int <| floor (daysSince1962Jan1 / 365.0)
   let year = years + 1962
   let daysSinceYear = daysSince1962Jan1 - float years * 365.0
   let months =
      ["Jan"; "Feb"; "Mar"; "Apr";
       "May"; "June"; "July"; "Aug";
       "Sep"; "Oct"; "Nov"; "Dec"]
   let month = months.[int (daysSinceYear * 12.0 / 365.0)]
   let lines =
      [
         sprintf "FPS: %A" (int fps);
         sprintf "%s%A" month year;
         sprintf "Zoom: %A (%A pixels/AU)" zoomModes.Head (int zoom);
         sprintf "Speed: %A days/second" (speed - 1.0);
         "";
         sprintf "Seek to: [%A]" seekYear;
         "Use number keys";
         "to enter a year";
         "to skip the";
         "simulation to.";
         "Press enter to go";
         "to this year.";
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
   for i = 0 to 2500 do
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
      if speed > 1.0 then
         speed <- speed / 2.0
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
   | Keys.D0 -> seekYear <- seekYear * 10
   | Keys.D1 -> seekYear <- seekYear * 10 + 1
   | Keys.D2 -> seekYear <- seekYear * 10 + 2
   | Keys.D3 -> seekYear <- seekYear * 10 + 3
   | Keys.D4 -> seekYear <- seekYear * 10 + 4
   | Keys.D5 -> seekYear <- seekYear * 10 + 5
   | Keys.D6 -> seekYear <- seekYear * 10 + 6
   | Keys.D7 -> seekYear <- seekYear * 10 + 7
   | Keys.D8 -> seekYear <- seekYear * 10 + 8
   | Keys.D9 -> seekYear <- seekYear * 10 + 9
   | Keys.Back -> seekYear <- seekYear / 10
   | Keys.Enter ->
      let since1962 = float seekYear - 1962.0
      let days = since1962 * 365.0
      let daysSinceJan20 = days - 19.0
      let newTime = daysSinceJan20 + 2439126.5
      let yearsSkipped = int ((newTime - time) / 365.0)
      for i = 1 to abs yearsSkipped do
         printfn "%A %A" i yearsSkipped
         let _ = solarSystem.Time (time + float (i * 365 * sign yearsSkipped))
         ()
      time <- newTime
   | _ -> ()
)
let clock = new System.Timers.Timer(1000.0 / 120.0)
let mutable lastTime = 0.0
clock.Elapsed.Add (fun e ->
   let nowTime = float e.SignalTime.Ticks / 10000000.0
   if lastTime <> 0.0 then
      dt <- nowTime - lastTime
      dtSmooth <- dtSmooth * 0.95 + dt * 0.05
      runSimulation ()
      form.Refresh ()
   lastTime <- nowTime
)
clock.Start()
Application.Run form