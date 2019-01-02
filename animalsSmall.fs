
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

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death.
  member this.resetHunger () =
    _hunger <- hungLen

/// give birth to a cub
  member this.giveBirth () =
    this.updateReproduction ()
    if this.reproduction <= 0 then
      Some (wolf (repLen, hungLen))
    else
      None

/// if the wolf is not dead, it calls update hunger and give birth
  member this.tick () : wolf option =
    match this.position with
    | Some position -> (this.updateHunger (); this.giveBirth ())
    | None -> None


/// A board is a chess-like board implicitly representedy by its width and
///coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int) =
  let _board : board = {
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen));
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen));
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

(*checkNabour returns a list with tuples with information on coordinates and
symbols.*)
  let checkNabour (b: board) (a: animal) =
    let arr = draw b /// draws the board with animals and empty fields
    let nc = /// list of neighbouring coordinates
      [(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)]
    // adding the the animal's position to neighbour coordinates
    let Neighbour = List.map (fun (x,y) ->
      (fst (Option.get a.position) + x, snd (Option.get a.position) + y)) nc
    let mutable nabour = List.empty<neighbour> // mutable empty list
    for k = 0 to (Neighbour.Length - 1) do
      let nx = (fst Neighbour.[k])
      let ny = (snd Neighbour.[k])
      // adds symbols to the coordinates and concats to the list nabour
      if nx > 0 && nx < _board.width && ny > 0 && ny < _board.width then
        nabour <- (Neighbour.[k], arr.[nx, ny]) :: nabour
    nabour

(* updateMoose examines if the moose should give birth to a calf, or if it
should change position.*)
  let updateMoose (b: board) (m: moose) =
    let someCalf = m.tick () // calls tick to check for a calf
    if m.position = None then
      ()
    else
      let list = (checkNabour b m) //list of neighbour position
      if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
        // newpos is a list of empty positions
        let newpos = List.find (fun ((_,_),x) -> x = eSymbol) (checkNabour b m)
        if someCalf <> None then
          let calf = (Option.get someCalf) // removes option from moose/calf
          calf.position <- Some (fst newpos) // position as coordinates
          _board.moose <- calf :: _board.moose // put calf on moose list
          m.resetReproduction() // reset reproduction to original length
        else
          m.position <- Some (fst newpos) //moose changes position
      else
        if someCalf <> None then
          m.resetReproduction() // reset reproduction to original length


(* updateWolf looks for a moose it can eat, if non, it gives a cub a position if
any, if non, it changes position. *)
  let updateWolf (b:board) (w: wolf) =
    let someCub = w.tick ()
    if w.position = None then
      ()
    else
      let list = (checkNabour b w)
      let anyMoose = // looks for any moose
        (List.exists (fun ((_,_),x) -> x = mSymbol) list)
      if anyMoose then
        let moosePos = // find moose position
          (List.find (fun ((_,_),x) -> x = mSymbol) list)
        w.position <- Some (fst moosePos) //Wolf goes to moose position
        for m in _board.moose do
          if m.position = Some (fst moosePos) then
            m.position <- None //Moose dies
        w.resetHunger () // Updates wolf hunger to original length
      elif someCub <> None then
        if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
          let newpos = // find first empty neighbour
            (List.find (fun ((_,_),x) -> x = eSymbol) list)
          let cub = (Option.get someCub) // Removes option frrom cub
          cub.position <- Some (fst newpos) //position as coordinates
          _board.wolves <- cub :: _board.wolves // cub in wolves list
          w.resetReproduction ()// reset reproduction to original length
        else
          (Option.get someCub).position <- None //cub dies of no empty position
          w.resetReproduction () // reset reproduction to original length
      else
        if (List.exists (fun ((_,_),x) -> x = eSymbol) list) then
          let newpos = // find first empty neighbour
            (List.find (fun ((_,_),x) -> x = eSymbol) list)
          w.position <- Some (fst newpos) //wolf moves position


(*processLists calls updateMoose and updateWolf and removes dead animals from
the lists.*)
  let rec processLists (mosList: moose List, wofList : wolf List) =
    let mutable mList = mosList
    let mutable wList = wofList

    let handleMoose m =
    //remove dead animals from moose board
      _board.moose <- List.filter (fun m -> m.position <> None) _board.moose
      mList <- List.filter (fun m -> m.position <> None) mList
      (updateMoose _board m)

    let handleWolf w =
    //remove dead animals from wolves board
      _board.wolves <- List.filter (fun w -> w.position <> None) _board.wolves
      wList <- List.filter (fun m -> m.position <> None) wList
      (updateWolf _board w)

    // The matchet chooses a random animals from the lists and processes it
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

// populate the board with animals placed at random.
  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

// bredden og højden
  member this.size = boardWidth*boardWidth
  member this.countMoose = _board.moose.Length
  member this.countWolf = _board.wolves.Length
  member this.board = _board
  member this.tick () =
    processLists (_board.moose, _board.wolves)
// test members of functions in scope of environment class
  member this.testBoard = _board

  member this.testMooseNabour =
    let moose = _board.moose.[1]
    printfn "checkNabour of random moose from isleRoyale over the course of
3 checkNabour calls:"
    for i = 1 to 3 do
      printfn "%A. run: %A" i (checkNabour _board moose)
      updateMoose _board moose

  member this.testWolfNabour =
    let wolf = _board.wolves.[1]
    printfn "checkNabour of random wolf from isleRoyale over the course of
3 checkNabour calls:"
    for i = 1 to 3 do
      printfn "%A. run: %A" i (checkNabour _board wolf)
      updateWolf _board wolf

  member this.testUpdateMoose =
    let moose = _board.moose.[1]
    printfn "Position and reproduction value of random moose from isleRoyale
over the course of 4 updateMoose calls:"
    for i = 1 to 4 do
    printfn "%A. update: Position: %A, Reproduction value: %A" i moose.position  moose.reproduction
    (updateMoose _board moose)

  member this.testUpdateWolf =
    let wolf = _board.wolves.[1]
    printfn "Position and reproduction value of random wolf from isleRoyale
over the course of 4 updateWolf calls:"
    for i = 1 to 4 do
    printfn "%A. update: Position: %A, Repro. value: %A, Hunger: %A" i wolf.position  wolf.position wolf.hunger
    (updateWolf _board wolf)

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
