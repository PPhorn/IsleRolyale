open System
open System.IO

[<EntryPoint >]
let main args =
  let ticks = if args.Length < 1 then 40 else int(args.[0])
  let fileName = if args.Length < 2 then "Pop_IsleRoyale" else args.[1]
  let boardW = if args.Length < 3 then 10 else int(args.[2])
  let moose = if args.Length < 4 then 30 else int(args.[3])
  let mooseRep = if args.Length < 5 then 10 else int(args.[4])
  let wolves = if args.Length < 6 then 2 else int(args.[5])
  let wolfRep = if args.Length < 7 then 10 else int(args.[6])
  let wolfHung = if args.Length < 8 then 4 else int(args.[7])

  if args <> Array.empty then
    printfn "Arguments given to the function %A" args
  else
    printfn "No arguments given. All arguments set to default values"

  let isle = animals.environment(boardW, moose, mooseRep, wolves, wolfRep, wolfHung)

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
