//#############################################
//assignment POP  10g.0 - signature and XML
//#############################################

module Animals

/// <summary>
///  Class Animal, creates a basic animal with a constructor. </summary>
/// <remarks> This class is abtract. </remarks>
/// <example>
///   <code>
///     inherit Animal
///   </code>
/// <param> Attributes of contructors is as showń below. <param>
[<AbstractClass>]
type Animal = class

    new : name:string * weight:int * maxSpeed:int -> Animal
    new : name:string * maxSpeed:int -> Animal

/// <summary> Method gets weight of an Animal instance. </summary>
/// <example>
///   <code>
///    let temp = obj.Weight
///   </code>
/// <returns> Returns weight as int. </returns>
    member Weight : int

/// <summary> Method gets speed of an Animal instance. </summary>
/// <example>
///   <code>
///    let temp = obj.Speed
///   </code>
/// <returns> Returns speed as int. </returns>
    member Speed : int

/// <summary> Method gets maxSpeed of an Animal instance. </summary>
/// <example>
///   <code>
///    let temp = obj.MaxSpeed
///   </code>
/// <returns> Returns maxSpeed as int. </returns>
    member MaxSpeed : int

/// <summary> Method gets and sets foodNeeded of an Animal instance. </summary>
/// <example>
///   <code>
///    let temp = obj.FoodNeeded
///    obj.FoodNeeded <- 100
///   </code>
/// <returns> Returns or sets foodNeeded as int. </returns>
    member FoodNeeded : int with get, set

/// <summary> Set speed of Animal proportionately to its food intake
///           and maximum speed. </summary>
/// <example>
///   <code>
///    obj.PassDay 50
///   </code>
/// <returns> Returns unit. </returns>
/// <param name="food"> The amount of food Animal should get.</param>
    member PassDay : food:int -> unit

/// <summary> Set the amount of food needed daily proportionately
///           to the animal’s weight </summary>
/// <example>
///   <code>
///    obj.DecideFoodNeeded
///   </code>
/// <returns> Returns unit. </returns>
    abstract member DecideFoodNeeded : unit -> unit
end


/// <summary>
///  Class Animal, creates a carnivore animal with a constructor. </summary>
/// <remarks> This class is inherit Animal and is private.
///           To create an instance use static member makeCarn. </remarks>
/// <example>
///   <code>
///    let cheetah = Carnivore.makeCarn ("Cheetah", 50, 114)
///   </code>
/// <param> Takes same attributes in contructore as the Animal class. <param>
type Carnivore = class
    inherit Animal

/// <summary> Creates instance of Carnivore. </summary>
/// <remarks> This method is static. </remarks>
/// <example>
///   <code>
///    let cheetah = Carnivore.makeCarn ("Cheetah", 50, 114)
///   </code>
/// <param> Arguments show below. <param>
    static member makeCarn : name:string * weight:int * maxSpeed:int -> Carnivore
end

/// <summary>
///  Class Animal, creates a herbivore animal with a constructor. </summary>
/// <remarks> This class is inherit Animal and is private.
///           To create an instance use static member makeHerb. </remarks>
/// <example>
///   <code>
///    let antelope = Herbivore.makeHerb ("Antelope", 50, 95)
///   </code>
/// <param> Takes same attributes in contructore as the Animal class. <param>
type Herbivore = class
    inherit Animal

/// <summary> Creates instance of Herbivore. </summary>
/// <remarks> This method is static. </remarks>
/// <example>
///   <code>
///    let antelope = Herbivore.makeHerb ("Antelope", 50, 95)
///   </code>
/// <param> Arguments show below. <param>
    static member makeHerb : name:string * weight:int * maxSpeed:int -> Herbivore
end

/// <summary> An Animal eat an random amount based on foodNeeded. </summary>
/// <remarks> value bewteen 1% to 100% proportionately to foodNeeded. </remarks>
/// <example>
///   <code>
///    let ate = eat obj
///   </code>
/// <param name="obj"> Animal instance. <param>
/// <returns> Return the procentage and amount of foodNeeded. <returns>
val eat : obj:Animal -> (int * int)

/// <summary> Animal runs n days, and is fed every day. </summary>
/// <remarks> A run is 10km. Prints out information of each run </remarks>
/// <example>
///   <code>
///    let winner = animals |> List.minBy (runNDays 3)
///   </code>
/// <param name="obj"> Animal instance. <param>
/// <param name="n"> How many days the it run 10km. <param>
/// <returns> Return the average time it took it to run as float. <returns>
val runNDays : n:int -> obj:Animal -> float

/// <summary> Prints the best average time for a list of Animal's.</summary>
/// <example>
///   <code>
///    race [rabbit;monket;ape;horse]
///   </code>
/// <param name="animals"> List of Animal's to race againt each other. <param>
/// <returns> Returns unit. <returns>
val race : animals:Animal list -> unit