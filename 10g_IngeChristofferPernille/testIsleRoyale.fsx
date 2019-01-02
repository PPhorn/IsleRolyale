let isleRoyale = animals.environment(10, 15, 5, 15, 5, 6)

printfn "\nTEST OF _board FUNCTION IN ENVIRONMENT SCOPE"
let boardTest = isleRoyale.testBoard
printfn "%A" boardTest

printfn "\nTEST OF checkNabour FUNCTION IN ENVIRONMENT SCOPE WITH ANIMAL-INPUT MOOSE"
let nabourMooseTest = isleRoyale.testMooseNabour
printfn "%A" nabourMooseTest

printfn "\nTEST OF checkNabour FUNCTION IN ENVIRONMENT SCOPE WITH ANIMAL-INPUT WOLF"
let nabourWolfTest = isleRoyale.testWolfNabour
printfn "%A" nabourWolfTest

printfn "\nTEST OF updateMoose FUNCTION IN ENVIRONMENT SCOPE"
let updateMooseTest = isleRoyale.testUpdateMoose
printfn "%A" updateMooseTest

printfn "\nTEST OF updatewolf FUNCTION IN ENVIRONMENT SCOPE"
let updateWolfTest = isleRoyale.testUpdateWolf
printfn "%A" updateWolfTest

printfn "\nTEST OF processLists FUNCTION IN ENVIRONMENT SCOPE"
let runProcessLists = isleRoyale.testProcessLists
printfn "%A" runProcessLists
