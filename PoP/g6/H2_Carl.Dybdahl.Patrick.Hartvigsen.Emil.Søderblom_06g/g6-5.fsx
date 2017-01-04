/// <summary>Checks that a colour is valid.</summary>
/// <param name="c">The colour to check in RGB format.</param>
/// <returns>Whether the colour is valid.</returns>
let checkColor (c: colour) : bool =
   match c with
   | (r, _, _) when r < 0 || r > 255 -> false
   | (_, g, _) when g < 0 || g > 255 -> false
   | (_, _, b) when b < 0 || b > 255 -> false
   | _ -> true

/// <summary>Check that a figure is valid.</summary>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (checkFigure (Circle ((0, 0), 10, (255, 0, 0))))
///   </code>
///   prints "true" to the console.
/// </example>
/// <param name="figure">The figure to check.</param>
/// <returns>Whether the figure is valid.</returns>
let rec checkFigure (fig : figure) : bool =
   match fig with
   | Circle (_, r, c) -> checkColor c && r >= 0
   | Rectangle ((x0, y0), (x1, y1), c) -> x1 >= x0 && y1 >= y0 && checkColor c
   | Mix (f1, f2) -> checkFigure f1 && checkFigure f2
   | Twice (f, _) -> checkFigure f


/// <summary>Computes the smallest rectangle that encloses the figure.</summary>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (boundingBox (Circle ((0, 0), 10, (255, 0, 0))))
///   </code>
///   prints "((-10, -10), (10, 10))" to the console.
/// </example>
/// <param name="figure">The figure to compute the bounding box of.</param>
/// <returns>The bounding box.</returns>
let rec boundingBox (fig : figure) : point * point =
   match fig with
   | Circle ((cx, cy), r, _) -> ((cx - r, cy - r), (cx + r, cy + r))
   | Rectangle (p0, p1, _) -> (p0, p1)
   | Mix (f1, f2) ->
      let ((b1x0, b1y0), (b1x1, b1y1)) = boundingBox f1
      let ((b2x0, b2y0), (b2x1, b2y1)) = boundingBox f2
      let minx = min b1x0 b2x0
      let miny = min b1y0 b2y0
      let maxx = max b1x1 b2x1
      let maxy = max b1y1 b2y1
      ((minx, miny), (maxx, maxy))
   | Twice (f, (dx, dy)) ->
      let ((x0, y0), (x1, y1)) = boundingBox f
      let minx = min x0 (x0 + dx)
      let miny = min y0 (y0 + dy)
      let maxx = max x1 (x1 + dx)
      let maxy = max y1 (y1 + dy)
      ((minx, miny), (maxx, maxy))