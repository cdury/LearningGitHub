// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.Drawing

let createDisposable name =
    printfn "creating: %s" name
    {new IDisposable with
        member x.Dispose() =
           printfn "disposing:%s " name
    }

let testDisposable() =
    use root = createDisposable "outer"
    for i in [1..2] do
        use nested = createDisposable "inner"
        printfn "comleting iteration %i" i
    printfn "leaving function"



module Drawing =
  let w, h = using (Image.FromFile(@"/Users/christian/Pictures/Wallpaper/dawn_in_antarctica_through_low_golden_brown_clouds_on_the_hills_above_the_ice_filled_sea.1920x1080.3ddb499c.jpg"))
                   (fun img -> (img.Width, img.Height))
  do
    printfn "Dimensions: %i x %i" w h

type Container() =
  member x.Fill ?stopAtPercent =
    printfn "%s" <| match (defaultArg stopAtPercent 0.5) with
                    | 1.0 -> "Filled it up"
                    | stopAt -> sprintf "Filled to %s" (stopAt.ToString("P2"))



[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    testDisposable()
    0 // return an integer exit code

