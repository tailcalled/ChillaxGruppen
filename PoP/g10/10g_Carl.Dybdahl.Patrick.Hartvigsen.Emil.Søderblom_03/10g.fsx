//#############################
// assignment POP 10g.0
//#############################

open Animals

let cheetah = Carnivore.makeCarn ("Cheetah", 50, 114)
let antelope = Herbivore.makeHerb ("Antelope", 50, 95)
let wildebeest = Herbivore.makeHerb ("Wildebeest", 200, 80)

let coerce (ani: Animal) = ani
let ani = [coerce cheetah;coerce antelope;coerce wildebeest]
race ani
//ani tro på os to, dududyyyy, ani tro på dig og mig dududyyyy,
//tro på os to og jeg tar' ikke FEJL!!! *guitar-funk*