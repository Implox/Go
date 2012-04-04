module Board

/// Tells whether a given space on the board is empty, occupied by a black piece, or occupied by a white piece.
/// Black is given the first "occupied" state because it is the color which moves first at the beginning of a game.
type CellState =
|Empty = 0
|Black = 1
|White = 2

/// Tells whether or not a given space on the board has been visited when searching for its liberties.
/// Used because determining the liberties of a given cell or group of cells requires a depth-first search algorithm.
type VisitState =
|Visited
|NotVisited

type ActiveBoard (sideLength : int) =
    let CellGrid = Array2D.create sideLength sideLength CellState.Empty

    /// Array of cells visited by the GetLiberties function
    /// Must be reset each time GetLiberties has been evaluated
    let VisitGrid = Array2D.create sideLength sideLength VisitState.NotVisited

    /// List of all the adjacent cells to a given stone.
    let Neighbors (x : int, y : int) = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] |> List.filter (fun (x, y) -> x >= 0 && x < sideLength && y >= 0 && y < sideLength)

    /// Gets the CellState of a given cell
    let GetCellState (x : int, y : int) = CellGrid.[x, y]

    /// Gets the liberties of a cell or group of cells
    let rec GetLiberties ((x : int, y : int)) =
        if GetCellState (x, y) <> CellState.Empty then
            VisitGrid.[x, y] <- VisitState.Visited
            let currentState = GetCellState (x, y)
            let GetVisitState (x : int, y : int) = VisitGrid.[x, y]
            let neighborList =
                (x, y) 
                |> Neighbors 
                |> List.filter (fun n -> GetCellState n = CellState.Empty || GetCellState n = currentState)
            let currentLiberties = neighborList |> List.filter (fun n -> GetCellState n = CellState.Empty)
            let currentNeighbors = neighborList |> List.filter (fun n -> GetCellState n <> CellState.Empty && GetVisitState n <> VisitState.Visited)
            match currentNeighbors with
            | [] -> currentLiberties
            | [_] -> currentLiberties @ GetLiberties currentNeighbors.[0]
            | _ -> currentLiberties @ List.collect GetLiberties currentNeighbors
        else []

    /// Method called when a player places one of their stones in a given cell of the board.
    member this.PlaceStone (x : int, y : int, state : CellState) =
        if CellGrid.[x, y] = CellState.Empty then CellGrid.[x, y] <- state
        else failwith "Cell already occupied"

    /// Returns the liberties of stone or group of stones
    member this.Liberties (x : int, y : int) = GetLiberties (x, y)
        

    member this.Grid = CellGrid
    member this.Width = sideLength
    member this.Height = sideLength


//Code below is a test for finding liberties and will be removed later.
open System

let board = new ActiveBoard 10
board.PlaceStone (3, 2, CellState.White)
board.PlaceStone (4, 2, CellState.White)
board.PlaceStone (5, 2, CellState.White)
board.PlaceStone (3, 3, CellState.White)
board.PlaceStone (4, 3, CellState.Black)
board.PlaceStone (5, 3, CellState.Black)
board.PlaceStone (6, 3, CellState.White)
board.PlaceStone (3, 4, CellState.White)
board.PlaceStone (4, 4, CellState.Black)
board.PlaceStone (5, 4, CellState.Black)

let defaultColor = Console.ForegroundColor
for j = 0 to 9 do
    for i = 0 to 9 do
        if board.Grid.[i, j] = CellState.Black then Console.ForegroundColor <- ConsoleColor.DarkGray
        elif board.Grid.[i, j] = CellState.White then Console.ForegroundColor <- ConsoleColor.White
        else Console.ForegroundColor <- defaultColor
        printf "%A " ((int)(board.Grid.[i, j]))
    printf "\n"

printf "Liberties: %A\n" (board.Liberties (3, 2))