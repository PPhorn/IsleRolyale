//10.0
type Counter() =
    let mutable _spent = 0
    member this.get() =
        _spent
    member this.incr() =
        _spent <- _spent + 1


//10.1
type Car (fueleff : int) =
    let _fueleff = float(fueleff)
    let mutable _fuel = 0.0
    member this.addGas(fuelToAdd: int) =
        _fuel <- _fuel + float(fuelToAdd)
    member this.gasLeft() =
        _fuel
    member this.drive(distance : int) =
        let _fuelcost = (float(distance) / _fueleff)
        if _fuel >= _fuelcost then
            _fuel <- _fuel - _fuelcost
        else
            failwith "Car doesn't have sufficient fuel"

//10.6
type Employee(name: string, nm: int) =
    let mutable _name = name
    let mutable _nm = nm

    member this.name = _name
    member this.updateName(name: string) =
        _name <- name
    member this.nm = _nm
    member this.updateNm(nm: int) =
        _nm <- nm

type ProductionWorker(name: string, nm: int) =
    inherit Employee(name, nm)
    let mutable _shiftnm = 1
    let mutable _hourpay = 0
    
    member this.shiftnm 
        with get() = _shiftnm

    member this.hourpay 
        with get() = _hourpay
    
    member this.updateShift(shift:int) =
        if shift = 1 || shift = 2 then
            _shiftnm <- shift
    member this.updatePay(pay:int) =
        _hourpay <- pay

type ShiftWorker(name: string, nm: int) =
    inherit Employee(name, nm)
    let mutable _shiftnm = 1
    let mutable _hourpay = 0
    
    member this.shiftnm 
        with get() = _shiftnm

    member this.hourpay 
        with get() = _hourpay
    
    member this.updateShift(shift:int) =
        if shift = 1 || shift = 2 then
            _shiftnm <- shift
    member this.updatePay(pay:int) =
        _hourpay <- pay

let a = ProductionWorker("Jakob",2)
printfn "%A, %A" a.shiftnm a.hourpay