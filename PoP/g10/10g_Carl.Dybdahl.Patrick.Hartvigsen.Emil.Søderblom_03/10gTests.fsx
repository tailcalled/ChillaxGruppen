//#################################################################
// assignment POP 10g.0 - test for PassDay and  DecideFoodNeeded
//#################################################################

open Animals

let testPassDay() =
    printfn "\nTesting PassDay..."
    let testAni = Carnivore.makeCarn ("Morty", 50, 20)
    let tPD food speed =
        testAni.PassDay(food)
        if testAni.Speed = speed
        then
            printfn "[ OK ] Speed was %A with %A food as expected"
                speed food
        else
            printfn "[FAIL] Speed was %A with %A food. Should've been %A"
                testAni.Speed food speed
    tPD 1 5
    tPD 2 10
    tPD 3 15
    tPD 4 20
testPassDay()

let testDecideFoodNeeded() =
    printfn "\nTesting decideFoodNeeded..."
    let testAni = Carnivore.makeCarn ("Jerry", 100, 10)
    //DecideFoodNeeded is automatically called by makeCarn
    if testAni.FoodNeeded = 8
    then printfn "[ OK ] Carnivore's DecideFoodNeeded worked as intented"
    else printfn "[FAIL] Carnivore's DecideFoodNeeded did not work as intented"
    let testHerbi = Herbivore.makeHerb ("Summer", 50, 8)
    //DecideFoodNeeded is automatically called by makeHerb
    if testHerbi.FoodNeeded = 20
    then printfn "[ OK ] Herbivore's DecideFoodNeeded worked as intented"
    else printfn "[FAIL] Herbivore's DecideFoodNeeded did not work as intented"
testDecideFoodNeeded()