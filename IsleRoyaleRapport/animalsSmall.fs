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
    |Some position -> (this.giveBirth ())
    |None -> None

    //None // Intentionally left blank. Insert code that updates the moose's age and optionally an offspring.

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    if _hunger <= 0 then
      this.position <- None // Starve to death
  member this.resetHunger () =
    _hunger <- hungLen

  member this.giveBirth () =
    this.updateReproduction ()
    if this.reproduction = 0 then
      Some (wolf (repLen, hungLen))
    else
      None

  member this.tick () : wolf option =
    match this.position with
    | Some position -> (this.updateHunger()); (this.giveBirth ())
    | None -> None

    // Intentionally left blank. Insert code that updates the wolf's age and optionally an offspring.
// updateHunger skal kaldes for hvert tick.
// Opdater ulvens alder.

/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting all rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
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

//
  let checkNabour (b: board) (a: animal): position = //ba er board array'et som sager _board som input
    let arr = draw b
    let NabourCord = [|(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)|]
    let mutable nabour = (0, 0)
    for k = 0 to (NabourCord.Length) do
      nabour <- a + (NabourCord.[k]) //returner en liste med tupler, der har informationer om naboerne, dvs. symbolerne på pladserne. Lav derefter en funktion, der håndtere situationen udfra, hvad der er i nabo koordinaterne.
      //while arr[i,j] <> eSymbol do
        //[(i,j)] <- NabourCord.[k + 1]
    (i,j)

  // populate the board with animals placed at random. Bruger anyEmptyFiels til at finde et frit koordinat.
  do for m in _board.moose do
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

<<<<<<< HEAD
=======
let updateMoose (b: _board) (m: moose) =
  if m.giveBirth () = Some then
    
  // indsæt baby på en plads rundt om.
  // update liste med en ny moose.
  else
  // flyt position et sted, hvor der er plads rundt om.

let updateWolf (w: wolf) =
  if //der er en ulv rundt om, spis den.
  elif m.giveBirth () = Some
  // indsæt baby på en plads rundt om.
  else
  w.hunger () = None
  // en ulv må der
  // den skal undersøge om den skal have en baby, spise en moose, eller dø.


let rec processLists (mList: moose List), (wList : wolf List) =
  let handleMoose m =
    (let calf, msg) = updateMoose _board m
  let handleWolf w =
    let (cub, msg) = updateWolf _ board
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


>>>>>>> 1949f165c69e8e0dd8c03ed8aa51e85d609744e4
// bredden og højden
  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.tick () =
    () // Intentionally left blank. Insert code that process animals here.
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

let newMoose = moose (3)
printfn "%A" (newMoose.reproduction)
