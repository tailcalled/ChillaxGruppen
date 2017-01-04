let gray = (128, 128, 128)
let convert (c : colour option) : int * int * int =
   match c with
   | None -> gray
   | Some (x) -> x

makeBMP.makeBMP "g63" 150 200 (fun (x, y) -> convert <| colourAt (x, y) g61)