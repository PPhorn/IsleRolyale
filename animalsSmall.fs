
module animals

type symbol = char
type position = int * int
type neighbour = position * symbol

let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let rnd = System.Random ()

/// An animal is a base class. It has a position and a reproduction counter.
type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen)
  let mutable _pos : position option = None
  let _symbol : symbol = symb

  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  override this.ToString () =
    string this.symbol

/// A moose is an animal
type moose (repLen : int) =
  inherit animal (mSymbol, repLen)

  member this.giveBirth () =
    this.updateReproduction ()
    if this.reproduction = 0 then
      Some (moose (repLen))
    else
      None

  member this.tick () : moose option =
    match this.position with
    | Some position -> (this.giveBirth ())
    | None -> None

/// Intentionally left blank. Insert code that updates the moose's age and optionally an offspring.

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death. position skal måske håndteres i
  member this.resetHunger () =
    _hunger <- hungLen

  member this.giveBirth () =
    this.updateReproduction ()
    if this.reproduction <= 0 then
      Some (wolf (repLen, hungLen))
    else
      None

// hvis ulven ikke dør, undersøger vi om den skal føder
  member this.tick () : wolf option =
    match this.position with
    | Some position -> (this.updateHunger (); this.giveBirth ())
    | None -> None


/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting all rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int) =
  let _board : board = {
    width = boardWidth;
    moose = List.init 2 (fun i -> moose(mooseRepLen));
    wolves = List.init 6 (fun i -> wolf(wolvesRepLen, wolvesHungLen));
  }

  /// Project the list representation of the board into a 2d array.
  let draw (b : board) : char [,] =
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol
    for m in b.moose do
      Option.iter (fun p -> arr.[fst p, snd p] <- mSymbol) m.position
    for w in b.wolves do
      Option.iter (fun p -> arr.[fst p, snd p] <- wSymbol) w.position
    arr

  /// return the coordinates of any empty field on the board.
  let anyEmptyField (b : board) : position =
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)

(*checkNabour returner en liste med tupler, der har informationer om naboerne, dvs.
symbolerne på pladserne. Lav derefter en funktion, der håndtere
situationen udfra, hvad der er i nabo koordinaterne.*)
  let checkNabour (b: board) (a: animal) =
    let arr = draw b
    let NC =
      [(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)]
    let Neighbour = List.map (fun (x,y) ->
      (fst (Option.get a.position) + x, snd (Option.get a.position) + y)) NC
    let mutable nabour = List.empty<neighbour>
    for k = 0 to (Neighbour.Length - 1) do
      let nx = (fst Neighbour.[k])
      let ny = (snd Neighbour.[k])
      if nx > 0 && nx < _board.width && ny > 0 && ny < _board.width then
        nabour <- (Neighbour.[k], arr.[nx, ny]) :: nabour
    nabour


(* updateMoose undersøger om moose skal have en kalv, eller om den skal skifte
 position. Indsæt baby på en plads rundt om via checkNabour.*)
  let updateMoose (b: board) (m: moose) =
    if m.position = None then
      ()
    else
      let someCalf = m.tick () //skal ikke være option
      let list = (checkNabour b m)
      if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
        let newpos = List.find (fun ((_,_),x) -> x = eSymbol) (checkNabour b m)
        if someCalf <> None then
          let calf = (Option.get someCalf) //Fjerner option fra moose/calf
          calf.position <- Some (fst newpos) //position er kun koordinatorne
          _board.moose <- calf :: _board.moose // Indsætter en calf på mooselisten
          m.resetReproduction()
        else
          m.position <- Some (fst newpos) //moose flytter position
      else
        if someCalf <> None then
          m.resetReproduction()


(* updateWolf undersøger om den kan spise en moose, om der er hvalp, eller om
den skal flytte position. *)
  let updateWolf (b:board) (w: wolf) =
    if w.position = None then
      ()
    else
      let someCub = w.tick ()
    // let cub = (Option.get someCub) // Fjerner option fra wolf/cub
      let list = (checkNabour b w)
      let anyMoose = // undersøger om der er mooses i nabofelt
        (List.exists (fun ((_,_),x) -> x = mSymbol) list)
      if anyMoose then
        let moosePos = // finder moosens position, så den kan spises
          (List.find (fun ((_,_),x) -> x = mSymbol) list)
        w.position <- Some (fst moosePos) //Ulven rykker hen på moosensplads
        for m in _board.moose do
          if m.position = Some (fst moosePos) then
            m.position <- None //Moosen dør
        w.resetHunger () // Opdaterer ulvens sultparameter
      //undersøger om der er et ledigt felt i nabofelt
      elif someCub <> None then
        if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
          let newpos = // finder førtse tomme position i nabofelt
            (List.find (fun ((_,_),x) -> x = eSymbol) list)
          let cub = (Option.get someCub) // Fjerner option fra wolf/cub
          cub.position <- Some (fst newpos) //position er kun koordinatorne
          _board.wolves <- cub :: _board.wolves // cub indsættes i wolveslisten
          w.resetReproduction () // reproductionen resettes
        else
          (Option.get someCub).position <- None //cub dør, da der ikke er nogen tomme nabofelter
          w.resetReproduction ()
      else
        if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
          let newpos = // finder første tomme position i nabofelt
            (List.find (fun ((_,_),x) -> x = eSymbol) list)
          w.position <- Some (fst newpos) //wolf flytter position


(*processLists kalder uodateMoose og updateWolf og fjerner eventuelle døde dyr
fra listerne.*)
  let rec processLists (mosList: moose List, wofList : wolf List) =
    let mutable mList = mosList
    let mutable wList = wofList

    let handleMoose m =
    //fjerner døde dyr fra mooseboard
      _board.moose <- List.filter (fun m -> m.position <> None) _board.moose
      mList <- List.filter (fun m -> m.position <> None) mList
      (updateMoose _board m)
      printfn "%A" _board.moose
    let handleWolf w =
    //fjerner døde dyr fra wolfboard
      _board.wolves <- List.filter (fun w -> w.position <> None) _board.wolves
      wList <- List.filter (fun m -> m.position <> None) wList
      (updateWolf _board w)
      printfn "%A" _board.wolves
// matchet vælger et tilfældigt dyr fra listerne og processeserer det.
    match (mList, wList) with
    | ([], []) -> ()
    | ([], w :: wList) -> handleWolf w
                          processLists ([], wList)
    | (m :: mList, []) -> handleMoose m
                          processLists (mList, [])
    | (m :: mList, w :: wList) -> if rnd.Next (2) = 1 then
                                    handleMoose m
                                    processLists (mList, w::wList)
                                  else
                                    handleWolf w
                                    processLists (m::mList, wList)

// populate the board with animals placed at random. Bruger anyEmptyFiels til at finde et frit koordinat.
  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

// bredden og højden
  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () =
    processLists (_board.moose, _board.wolves) // Intentionally left blank. Insert code that process animals here.
// test members of functions in scope of environment class
  member this.testBoard = _board
  member this.testMooseNabour =
    let moose = _board.moose.[1]
    checkNabour _board moose
  member this.testWolfNabour =
    let wolf = _board.wolves.[1]
    checkNabour _board wolf
  member this.testUpdateMoose =
    let moose = _board.moose.[1]
    printfn "1.Position and reproduction value of random moose from isleRoyale:"
    printfn "- Position: %A" moose.position
    printfn "- Reproduction value: %A" moose.reproduction
    printfn "2.updateMoose run - function return: %A" (updateMoose _board moose)
    printfn "3.Position and reproduction level of same moose after 1 updateMoose:"
    printfn "- Position: %A" moose.position
    printfn "- Reproduction value: %A" moose.reproduction
    (updateMoose _board moose)
    printfn "4. Position and reproduction level of same moose after 2 updateMoose:"
    printfn "- Position: %A" moose.position
    printfn "- Reproduction value: %A" moose.reproduction
    (updateMoose _board moose)
    printfn "5. Position and reproduction level of same moose after 3 updateMoose:"
    printfn "- Position: %A" moose.position
    printfn "- Reproduction value: %A" moose.reproduction

  member this.testUpdateWolf =
    let wolf = _board.wolves.[1]
    printfn "1.Position and reproduction value of random wolf from isleRoyale:"
    printfn "- Position: %A" wolf.position
    printfn "- Reproduction value: %A" wolf.reproduction
    printfn "2.updateWolf run - function return: %A" (updateWolf _board wolf)
    printfn "3.Position and reproduction leves of same wolf after 1 updateWolf:"
    printfn "- Position: %A" wolf.position
    printfn "- Reproduction value: %A" wolf.reproduction
    (updateWolf _board wolf)
    printfn "4. Position and reproduction level of same wolf after 2 updateWolf:"
    printfn "- Position: %A" wolf.position
    printfn "- Reproduction value: %A" wolf.reproduction
    (updateWolf _board wolf)
    printfn "5. Position and reproduction level of same wolf after 3 updateWolf:"
    printfn "- Position: %A" wolf.position
    printfn "- Reproduction value: %A" wolf.reproduction

  member this.testProcessLists =
    processLists (_board.moose, _board.wolves)

  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret
