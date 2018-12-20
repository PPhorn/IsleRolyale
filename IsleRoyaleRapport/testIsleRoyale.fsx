let isleRoyale = animals.environment(10, 6, 5, 3, 5, 6, false)

printfn "Test of _board function in environment scope:"
let boardTest = isleRoyale.testBoard
printfn "%A" boardTest
