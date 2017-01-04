type weekday = Monday | Tuesday | Wednesday | Thursday | Friday
             | Saturday | Sunday

/// <summary>
///  Converts a number <code>1 .. 7</code> to a day
///  <code>Monday .. Friday</code>
/// </summary>
/// <remarks>
///  This function returns a value of type <code>weekday option</code>. This is
///  so it can return <code>None</code> if the input is not in the allowed
///  interval. 
/// </remarks>
/// <example>
///   The following code:
///   <code>
///     printfn "%A" (numberToDay 1)
///   </code>
///   prints "Some Monday" to the console.
/// </example>
/// <param name="n">The index of the weekday.</param>
/// <returns>The weekday.</returns>
let numberToDay n =
   match n with
    | 1 -> Some Monday
    | 2 -> Some Tuesday
    | 3 -> Some Wednesday
    | 4 -> Some Thursday
    | 5 -> Some Friday
    | 6 -> Some Saturday
    | 7 -> Some Sunday
    | _ -> None