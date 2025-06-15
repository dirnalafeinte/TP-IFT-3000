namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}
    
    // Obtient la taille d'un bateau selon sa classe
    let getShipSize (name: Name) : int =
        match name with
        | Spy -> 2
        | PatrolBoat -> 2
        | Destroyer -> 3
        | Submarine -> 3
        | Cruiser -> 4
        | AircraftCarrier -> 5
    
    // Calcule la position du centre dans la liste de coordonnées
    let getCenterIndex (size: int) : int =
        (size / 2) - 1
    
    // Calcule le décalage pour une direction donnée
    let getDirectionOffset (direction: Direction) : Coord =
        match direction with
        | North -> (-1, 0)
        | South -> (1, 0)
        | East -> (0, 1)
        | West -> (0, -1)
    
    // Additionne deux coordonnées
    let addCoords (coord1: Coord) (coord2: Coord) : Coord =
        let (i1, j1) = coord1
        let (i2, j2) = coord2
        (i1 + i2, j1 + j2)
    
    // Multiplie une coordonnée par un scalaire
    let multiplyCoord (coord: Coord) (scalar: int) : Coord =
        let (i, j) = coord
        (i * scalar, j * scalar)

    (* --- Nouvelles fonctions --- *)

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let size = getShipSize name
        let centerIndex = getCenterIndex size
        let offset = getDirectionOffset facing
        
        // Génère les coordonnées du bateau
        let rec generateCoords index acc =
            if index >= size then List.rev acc
            else
                let displacement = index - centerIndex
                let coordOffset = multiplyCoord offset displacement
                let coord = addCoords center coordOffset
                generateCoords (index + 1) (coord :: acc)
        
        let coords = generateCoords 0 []
        
        { Coords = coords; Center = center; Facing = facing; Name = name }

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        let (rows, cols) = dims
        
        // Trouve les coordonnées min/max du bateau
        let minRow = List.fold (fun acc (i, _) -> min acc i) System.Int32.MaxValue ship.Coords
        let maxRow = List.fold (fun acc (i, _) -> max acc i) System.Int32.MinValue ship.Coords
        let minCol = List.fold (fun acc (_, j) -> min acc j) System.Int32.MaxValue ship.Coords
        let maxCol = List.fold (fun acc (_, j) -> max acc j) System.Int32.MinValue ship.Coords
        
        // Génère toutes les coordonnées du rectangle périmètre
        let rec generatePerimeterCoords i j acc =
            if i > (maxRow + 1) then acc
            elif j > (maxCol + 1) then generatePerimeterCoords (i + 1) (minCol - 1) acc
            else
                let coord = (i, j)
                let isInBounds = i >= 0 && i < rows && j >= 0 && j < cols
                let isShipCoord = List.contains coord ship.Coords
                let isPerimeterCoord = 
                    (i = (minRow - 1) || i = (maxRow + 1) || 
                     j = (minCol - 1) || j = (maxCol + 1)) &&
                    i >= (minRow - 1) && i <= (maxRow + 1) &&
                    j >= (minCol - 1) && j <= (maxCol + 1)
                
                if isInBounds && not isShipCoord && isPerimeterCoord then
                    generatePerimeterCoords i (j + 1) (coord :: acc)
                else
                    generatePerimeterCoords i (j + 1) acc
        
        generatePerimeterCoords (minRow - 1) (minCol - 1) []