module Planets
open Geom

/// A type representing a planet
type Planet = class
   
   /// The position of the planet
   member Pos : V3
   /// The velocity of the planet
   member Vel : V3
   /// The mass of the planet
   member Mass : float
   /// The name of the planet
   member Name : string

   /// Constructs a planet given position, velocity, mass and name.
   new : pos:V3 * vel:V3 * mass:float * name:string -> Planet

end

/// Represents a position entry in a JPLE planet file
type PlanetEntry = float * float * float * float * float
/// Represents the data for a JPLE planet
type PlanetData = string * float * PlanetEntry list

/// Reads the JPL Epheremis data for a given planet
val readPlanet : string -> PlanetData

/// The time of a planet entry
val entryTime : PlanetEntry -> float
/// The position of a planet entry
val computePosition : PlanetEntry -> V3

/// Contains the name of each planet
val planetNames : string list

/// Constructs a list of planets using the given planet data and a starting
/// time. Will pick data as close as possible to the given starting time.
val constructPlanets : PlanetData list -> float -> float * Planet list

/// The data in the JPLE files
val planetData :
   (string * float * (float * float * float * float * float) list) list
/// The planets at the earliest time
val planets : Planet list
/// The earliest time
val t0 : float
/// The sun
val sol : Planet

/// Simulates a system of planets for a given amount of time
val simulate : float -> Planet list -> Planet list

/// Convenience class to manipulate a simulated solar system
type SolarSystem = class

   /// Constructs a solar system using the planet list with base time initTime
   new : initSystem:Planet list * initTime:float -> SolarSystem
   /// Constructs a solar system using the JPLE data closest to the given time
   new : time:float -> SolarSystem

   /// Looks up the simulated state of the system at the given time
   member Time : float -> Planet list
   /// The base time of the solar system, guaranteed to match JPLE
   member T0 : float

end