namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    let isCoordOccupied (coord: Coord) (grid: Sector Grid) : bool =
        match Grid.get coord grid with
        | Some(Active(_, _)) -> true
        | _ -> false
    
    let isCoordInPerimeter (coord: Coord) (excludeShip: Ship option) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        
        let shipNames = 
            Grid.fold (fun acc shipCoord sector ->
                match sector with
                | Active(name, _) -> 
                    let shouldExclude = 
                        match excludeShip with
                        | Some(excludedShip) -> name = excludedShip.Name
                        | None -> false
                    
                    if shouldExclude || List.contains name acc then acc
                    else name :: acc
                | _ -> acc
            ) [] grid
        
        List.exists (fun shipName ->
            let shipCoords = 
                Grid.fold (fun coordAcc coord2 sector2 ->
                    match sector2 with
                    | Active(name2, index) when name2 = shipName -> (coord2, index) :: coordAcc
                    | _ -> coordAcc
                ) [] grid
                |> List.sortBy snd
                |> List.map fst
            
            if List.length shipCoords > 0 then
                let size = List.length shipCoords
                let centerIndex = Ship.getCenterIndex size
                let center = List.item centerIndex shipCoords
                let direction = 
                    if size > 1 then
                        let first = List.head shipCoords
                        let second = List.item 1 shipCoords
                        let (di, dj) = Ship.addCoords second (Ship.multiplyCoord first (-1))
                        if di = 0 && dj = 1 then East
                        elif di = 0 && dj = -1 then West
                        elif di = 1 && dj = 0 then South
                        else North
                    else North
                
                let tempShip = { Coords = shipCoords; Center = center; Facing = direction; Name = shipName }
                let perimeter = Ship.getPerimeter tempShip dims
                List.contains coord perimeter
            else false
        ) shipNames


    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let ship = Ship.createShip center direction name
        
        List.forall (fun coord ->
            Grid.isInBounds coord dims &&
            not (isCoordOccupied coord grid) &&
            not (isCoordInPerimeter coord None grid)
        ) ship.Coords

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let offset = Ship.getDirectionOffset direction
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        
        List.forall (fun coord ->
            Grid.isInBounds coord dims &&
            match Grid.get coord grid with
            | Some(Active(name, _)) -> name = ship.Name
            | _ -> true
        ) newCoords

    let move (ship: Ship) (direction: Direction) : Ship =
        let offset = Ship.getDirectionOffset direction
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        let newCenter = Ship.addCoords ship.Center offset
        
        { ship with Coords = newCoords; Center = newCenter }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let newShip = Ship.createShip ship.Center direction ship.Name
        
        List.forall (fun coord ->
            Grid.isInBounds coord dims &&
            match Grid.get coord grid with
            | Some(Active(name, _)) -> name = ship.Name
            | _ -> true
        ) newShip.Coords

    let rotate (ship: Ship) (direction: Direction) : Ship =
        Ship.createShip ship.Center direction ship.Name

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let offset = Ship.getDirectionOffset ship.Facing
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        
        let results = 
            List.map (fun coord ->
                let inBounds = Grid.isInBounds coord dims
                let occupied = 
                    match Grid.get coord grid with
                    | Some(Active(name, _)) -> name <> ship.Name
                    | _ -> false 
                (coord, inBounds, occupied)
            ) newCoords
        
        List.forall (fun coord ->
            Grid.isInBounds coord dims &&
            match Grid.get coord grid with
            | Some(Active(name, _)) -> name = ship.Name
            | _ -> true
        ) newCoords

    let moveForward (ship: Ship) : Ship =
        move ship ship.Facing

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        match rotation with
        | Clockwise ->
            match current with
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | Counterclockwise ->
            match current with
            | North -> West
            | West -> South
            | South -> East
            | East -> North

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        let newDirection = getNextDirection ship.Facing rotation
        let rotatedShip = Ship.createShip ship.Center newDirection ship.Name
        let dims = Grid.getDimensions grid
        let offset = Ship.getDirectionOffset newDirection
        let finalCoords = List.map (Ship.addCoords offset) rotatedShip.Coords
        
        List.forall (fun coord ->
            Grid.isInBounds coord dims &&
            not (isCoordOccupied coord grid)
        ) finalCoords

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let newDirection = getNextDirection ship.Facing rotation
        let rotatedShip = Ship.createShip ship.Center newDirection ship.Name
        moveForward rotatedShip