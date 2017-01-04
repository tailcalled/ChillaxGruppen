let dayToNumber (d : weekday) : int =
   match d with
   | Monday -> 1
   | Tuesday -> 2
   | Wednesday -> 3
   | Thursday -> 4
   | Friday -> 5
   | Saturday -> 6
   | Sunday -> 7

let days = [Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday]

if days |> List.forall (fun d -> numberToDay <| dayToNumber d = Some d) then
   printfn "[ OK ] numberToDay (dayToNumber d) = Some d"
else
   printfn "[FAIL] numberToDay (dayToNumber d) != Some d"

let testsNumToDay = [(0, None); (1, Some Monday); (10, None); (7, Some Sunday)]

for (input, expected) in testsNumToDay do
   let result = numberToDay input
   if result = expected then
      printfn "[ OK ] numberToDay %A = %A" input expected
   else
      printfn "[FAIL] numberToDay %A = %A != %A" input result expected

let circ = Circle ((0, 0), 10, (255, 0, 0))
let rect = Rectangle ((0, 0), (5, 5), (0, 255, 0))

let testsColourAt = [
   ((0, 0), circ, Some (255, 0, 0));
   ((10, 10), circ, None);
   ((0, 0), rect, Some (0, 255, 0));
   ((10, 10), rect, None);
   ((0, 0), Mix (circ, rect), Some (127, 127, 0));
   ((6, 6), Mix (circ, rect), Some (255, 0, 0));
   ((0, 0), Twice (circ, (0, 100)), Some (255, 0, 0));
   ((0, 100), Twice (circ, (0, 100)), Some (255, 0, 0));
   ((0, 50), Twice (circ, (0, 100)), None)
]

for (inPos, inFig, expected) in testsColourAt do
   let result = colourAt inPos inFig
   if result = expected then
      printfn "[ OK ] colourAt %A %A = %A" inPos inFig expected
   else
      printfn "[FAIL] colourAt %A %A = %A != %A" inPos inFig result expected

let correctFigs = [
   circ;
   rect;
   g61;
   Circle ((0, 0), 0, gray);
   Mix (circ, rect);
   Twice (circ, (10, 10))
]
let incorrectFigs = [
   Circle ((0, 0), -10, (255, 0, 0));
   Rectangle ((5, 5), (0, 0), (0, 255, 0));
   Circle ((0, 0), 5, (-1, -1, -1))
]

for fig in correctFigs do
   if checkFigure fig then
      printfn "[ OK ] checkFigure %A = true" fig
   else
      printfn "[FAIL] checkFigure %A = false" fig

for fig in incorrectFigs do
   if checkFigure fig then
      printfn "[FAIL] checkFigure %A = true" fig
   else
      printfn "[ OK ] checkFigure %A = false" fig

let testsBoundingBox = [
   (o61, ((5, 5), (95, 110)));
   (circ, ((-10, -10), (10, 10)));
   (rect, ((0, 0), (5, 5)));
   (Mix (circ, o61), ((-10, -10), (95, 110)));
   (Twice (circ, (10, 0)), ((-10, -10), (20, 10)))
]

for (input, expected) in testsBoundingBox do
   let result = boundingBox input
   if result = expected then
      printfn "[ OK ] boundingBox %A = %A" input expected
   else
      printfn "[FAIL] boundingBox %A = %A != %A" input result expected