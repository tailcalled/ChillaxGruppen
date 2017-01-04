type point = int * int
type colour = int * int * int
type figure =
    | Circle of point * int * colour
    | Rectangle of point * point * colour
    | Mix of figure * figure
    | Twice of figure * (int * int)

let o61 =
   let circ = Circle ((50, 50), 45, (255, 0, 0))
   let rect = Rectangle ((40, 40), (90, 110), (0, 0, 255))
   Mix (circ, rect)

let g61 = Twice (o61, (50, 70))
