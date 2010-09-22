#light

open System
open System.Collections.Generic

type MathOps=        
    static member ToRadian a: float=
        a*Math.PI/180.0

type Direction = 
    | N = 90 
    | E = 0
    | S = 270
    | W = 180
    
type rover (x: float, y: float, theta: int)=
    member this.X = x
    member this.Y = y
    member this.Theta = theta
    member this.TurnLeft=
        new rover(this.X,this.Y ,(this.Theta + 90) % 360)
    member this.TurnRight=
        new rover(this.X,this.Y ,(this.Theta - 90) % 360 )
    member this.Move =
        new rover(this.X + Math.Cos(MathOps.ToRadian(float this.Theta)),this.Y + Math.Sin(MathOps.ToRadian(float this.Theta)),this.Theta)
    member this.PrintState=
        String.Concat(this.X.ToString(), " " , this.Y.ToString() , " " , (Enum.GetName (typeof<Direction> ,this.Theta )))
    member this.ProcessSignal (signal: string)=
        let rov=
            match signal.ToCharArray().[0] with
                    | 'L' -> this.TurnLeft
                    | 'R' -> this.TurnRight
                    | 'M' -> this.Move
                    | _ -> this
        match signal.Length with
            | 1 -> rov
            | _ -> rov.ProcessSignal(signal.Substring(1,signal.Length-1))
            
type plateau(x : int, y : int) =
    let RoverList = new List<rover>()
    let rec PrintRover (list: List<rover>)= 
        match list.Count with
        | 0 -> ""
        | 1 -> list.[0].PrintState
        | x -> String.Concat( list.[0].PrintState, PrintRover(list.GetRange(1,x-1)),"\n")
    member this.AddRover (rov : rover) =
        RoverList.Add(rov)
    member this.X = x
    member this.Y = y
    member this.PrintRoverPosition=
        PrintRover RoverList


Console.WriteLine("Enter Plateau's top right co-ordinates:")
let plateauCoordinates = Console.ReadLine().Split(' ')
let gamePlateau = new plateau(int plateauCoordinates.[0],int plateauCoordinates.[1])

let mutable roverPos = " "
let mutable roverSignal = ""

while roverPos <> "" do
    Console.WriteLine("Enter Rover's initial position")
    roverPos <- Console.ReadLine()
    if roverPos <> "" then
        let roverCoord = roverPos.Split(' ')
        let roverDir = 
            match roverCoord.[2] with 
                | "N" -> Direction.N
                | "S" -> Direction.S
                | "E" -> Direction.E
                | "W" -> Direction.W
                | _ -> failwith "Invalid direction"
        Console.WriteLine("Enter Rover's signal")
        let roverSignal =Console.ReadLine()
        let rover1 = (new rover(float roverCoord.[0],float roverCoord.[1],int roverDir)).ProcessSignal(roverSignal )
        gamePlateau.AddRover(rover1)

printf "Final position of Rovers:\n%s" gamePlateau.PrintRoverPosition 
Console.ReadLine() |> ignore