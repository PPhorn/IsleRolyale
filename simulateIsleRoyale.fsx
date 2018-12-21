[<EntryPoint >]
let main args =
  let ticks = if args.Length < 1 then 40 else int(args.[0])
  let fileName = if args.Length < 1 then "filename" else args.[1]
  let boardW = if args.Length < 1 then 10 else int(args.[2])
  let moose = if args.Length < 1 then 15 else int(args.[3])
  let mooseRep = if args.Length < 1 then 3 else int(args.[4])
  let wolves = if args.Length < 1 then 2 else int(args.[5])
  let wolfRep = if args.Length < 1 then 4 else int(args.[6])
  let wolfHung = if args.Length < 1 then 4 else int(args.[7])

  //printfn "Arguments given to the function %A" args


  // let isle = animals.environment(boardW, moose, mooseRep, wolves, wolfRep, wolfHung, false)
  let isle = animals.environment(10, 4, 3, 2, 3, 10, false)
  //let file = nkadnæbEJ "%A" filename

  for i = 1 to 40 do
    printfn "%A" isle
    isle.tick ()


  0
