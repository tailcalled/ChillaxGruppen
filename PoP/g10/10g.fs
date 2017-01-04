//##################################
//assignment POP 10g.0 - module
//##################################

module Animals
//Instance of Random
let rnd = System.Random()

//abstract Animal class
[<AbstractClass>]
type Animal  (name : string, weight : int, maxSpeed : int) = class
    let mutable speed = 0
    let mutable foodNeeded = 0

    //Aditionel contrictor, if weight is not given.
    //Not in usefull because there is no makeCarn and makeHerb for this contructor.
    new(name : string, maxSpeed : int) =
        Animal(name, rnd.Next(70,301), maxSpeed)

    member this.Name = name
    member this.Weight = weight
    member this.Speed = speed
    member this.MaxSpeed = maxSpeed
    member this.FoodNeeded
        with get () = foodNeeded
        and set value = foodNeeded <- value

    //Changes current speed
    member this.PassDay (food : int) = speed <- (maxSpeed * food) / foodNeeded

    //Calculate foodNeeded, can be overrided by subclasses
    abstract member DecideFoodNeeded : unit -> unit
    default this.DecideFoodNeeded() = foodNeeded <- weight/2
end

//Carnivore class, inherit Animal
type Carnivore private (name : string, weight : int, maxSpeed : int) = class
    inherit Animal(name, weight, maxSpeed)
    override this.DecideFoodNeeded() = this.FoodNeeded <- (weight*8)/100
    static member makeCarn (name, weight, maxSpeed) =
        let carn = new Carnivore (name, weight, maxSpeed)
        carn.DecideFoodNeeded()
        carn
end

//Herbivore class, inherit Animal
type Herbivore private (name : string, weight : int, maxSpeed : int) = class
    inherit Animal(name, weight, maxSpeed)
    override this.DecideFoodNeeded() = this.FoodNeeded <- (weight*40)/100
    static member makeHerb (name, weight, maxSpeed) =
        let herb = new Herbivore (name, weight, maxSpeed)
        herb.DecideFoodNeeded()
        herb
end

//Animal eats a days meal, current speed changes
let eat (obj : Animal) =
    let rndP = rnd.Next(1,101)
    let amount = (obj.FoodNeeded*rndP/100)
    obj.PassDay amount
    (rndP, amount)

//Run n days and 10 km each day with current speed
let runNDays (n : int) (obj : Animal) =
    [1..n] |> List.averageBy (fun x ->
        let ate = eat obj
        let dist = 10.0/float obj.Speed
        printfn "%s ate %A%% of %A kg. That is equal to %A kg."
            obj.Name (fst ate) obj.FoodNeeded (snd ate)
        printfn "It  took it %A hours to travel 10 km.\n" dist
        dist )

//Race a list of Animals for 3 days, average best-time wins
let race (animals : Animal list) =
    let winner = animals |> List.minBy (runNDays 3)
    printfn "The %s won the race" winner.Name