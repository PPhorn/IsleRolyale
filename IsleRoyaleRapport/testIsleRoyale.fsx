let isleRoyale = isle.environment(10, 6, 5, 3, 5, 6, false)


printfn "Test of checkNabour function in environment scope:"
let testMoose = moose (3)
let testNeighbour = isleRoyale.testNabour (testMoose)
printf "%A" testNeighbour
