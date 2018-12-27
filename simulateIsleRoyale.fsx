open System
open System.IO

[<EntryPoint >]
let main args =
  let ticks = if args.Length < 1 then 40 else int(args.[0])
  let fileName = if args.Length < 1 then "Pop_IsleRoyale" else args.[1]
  let boardW = if args.Length < 1 then 10 else int(args.[2])
  let moose = if args.Length < 1 then 15 else int(args.[3])
  let mooseRep = if args.Length < 1 then 3 else int(args.[4])
  let wolves = if args.Length < 1 then 4 else int(args.[5])
  let wolfRep = if args.Length < 1 then 3 else int(args.[6])
  let wolfHung = if args.Length < 1 then 4 else int(args.[7])
//  let givenArray = [|ticks; boardW; moose; mooseRep|]


  printfn "Arguments given to the function %A" args
  //printfn "%A" args.[0]

  let isle = animals.environment(boardW, moose, mooseRep, wolves, wolfRep, wolfHung)
  //let isle = animals.environment(10, 4, 3, 2, 3, 9)
  //let en fil

  let mutable popCount = "T" + "," + "M" + "," + "W" + "\n" + string 0 + "," + string isle.countMoose + "," + string isle.countWolf

  for i = 1 to ticks do
    printfn "%A" isle
    isle.tick ()
    let theTick = i
    popCount <- popCount + "\n" + string theTick + "," + string isle.countMoose + "," + string isle.countWolf
  printfn "%s" popCount

  use textFile = File.CreateText (fileName + ".csv")
  textFile.Write popCount

  0
