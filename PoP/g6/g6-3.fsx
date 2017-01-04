/// <summary>Finds the colour at a position in a figure.</summary>
/// <remarks>
///  May take exponential time if the figure has many recursively nested
///  <code>Twice</code> constructors. If the point is outside the area of the
///  figure, the function returns <code>None</code>.
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (colourAt (0, 0) (Circle ((0, 0), 10, (255, 0, 0))))
///   </code>
///   prints "Some (255, 0, 0)" to the console.
/// </example>
/// <param name="(x, y)">The coordinates to find the colour at.</param>
/// <param name="figure">The figure to find the colour of.</param>
/// <returns>The colour at the point on the figure.</returns>
let rec colourAt (x, y) figure =
   match figure with
   | Circle ((cx, cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
      then Some col else None
   | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0<=x && x <= x1 && y0 <= y && y <= y1
      then Some col else None
   | Mix (f1, f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c // overlapper ikke
      | (c, None) -> c // ditto
      | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
         Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)
   | Twice (f1, (dx, dy)) ->
      match (colourAt (x, y) f1, colourAt (x-dx,y-dy) f1) with
      | (c, None) -> c
      | (_, Some c2) -> Some c2