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

let mutable solarSystem = sol :: planets
let form = new PlanetsForm()
let mutable dt = 0.0
let mutable dtSmooth = 0.0
let font = new Font("Arial", 16.0F)
let mutable zoom = 35.0F // pixels per AU

let paint (gfx : Graphics) =
   let width = form.ClientSize.Width
   let height = form.ClientSize.Height
   gfx.FillRectangle (Brushes.Black, 0, 0, width, height)
   let fps = 1.0 / dtSmooth
   gfx.DrawString (sprintf "FPS: %A" fps, font, Brushes.White, 0.0F, 0.0F)
   let rand = new System.Random(5918421)
   for i = 0 to 1000 do
      let x = rand.Next(width)
      let y = rand.Next(height)
      gfx.FillRectangle (Brushes.White, x, y, 1, 1)
   gfx.TranslateTransform(float32 width / 2.0F, float32 height / 2.0F)
   for planet in solarSystem do
      let x = float32 planet.Pos.X * zoom
      let y = float32 planet.Pos.Y * zoom
      let sz = log (2.714F * log (float32 planet.Mass * 2.714F / 1.3E22F)) * 2.0F
      let b = if planet.Name = "Sol" then Brushes.Yellow else Brushes.Gray
      gfx.FillEllipse(b, x - sz, y - sz, 2.0F * sz, 2.0F * sz)
      gfx.DrawString (planet.Name, font, Brushes.White, x, y)

let speed = 1
let runSimulation () =
   for i = 1 to speed do
      solarSystem <- simulate dt solarSystem

form.Paint.Add (fun e ->
   paint e.Graphics
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