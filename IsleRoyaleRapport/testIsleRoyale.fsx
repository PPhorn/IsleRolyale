let isleRoyale = animals.environment(10, 15, 5, 15, 5, 6, false)

printfn "\nTest of _board function in environment scope:"
let boardTest = isleRoyale.testBoard
printfn "%A" boardTest

printfn "\nTest of checkNabour function in environment scope with animal-input moose:"
let nabourMooseTest = isleRoyale.testMooseNabour
printfn "%A" nabourMooseTest

printfn "\nTest of checkNabour function in environment scope with animal-input wolf:"
let nabourWolfTest = isleRoyale.testWolfNabour
printfn "%A" nabourWolfTest

printfn "\nTest of updateMoose function in environment scope:"
let updateMooseTest = isleRoyale.testUpdateMoose
printfn "%A" updateMooseTest

printfn "\nTest of updatewolf function in environment scope:"
let updateWolfTest = isleRoyale.testUpdateWolf
printfn "%A" updateWolfTest
