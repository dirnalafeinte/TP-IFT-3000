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

    (* Fonctions helper *)
    
    // Vérifie si une coordonnée est occupée par un bateau dans la grille
    let isCoordOccupied (coord: Coord) (grid: Sector Grid) : bool =
        match Grid.get coord grid with
        | Some(Active(_, _)) -> true
        | _ -> false
    
    // Vérifie si une coordonnée fait partie du périmètre d'autres bateaux
    let isCoordInPerimeter (coord: Coord) (excludeShip: Ship option) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        
        // Trouve tous les bateaux dans la grille (sauf celui exclu)
        let ships = 
            Grid.fold (fun acc shipCoord sector ->
                match sector with
                | Active(name, index) when index = 0 -> // Premier segment du bateau
                    // Reconstruit le bateau depuis la grille
                    let shipCoords = 
                        Grid.fold (fun coordAcc coord2 sector2 ->
                            match sector2 with
                            | Active(name2, _) when name2 = name -> coord2 :: coordAcc
                            | _ -> coordAcc
                        ) [] grid
                        |> List.sort
                    
                    if List.length shipCoords > 0 then
                        // Trouve le centre et la direction du bateau
                        let center = List.item (Ship.getCenterIndex (List.length shipCoords)) shipCoords
                        let direction = 
                            if List.length shipCoords > 1 then
                                let first = List.head shipCoords
                                let second = List.item 1 shipCoords
                                let (di, dj) = Ship.addCoords second (Ship.multiplyCoord first (-1))
                                if di = 0 && dj = 1 then East
                                elif di = 0 && dj = -1 then West
                                elif di = 1 && dj = 0 then South
                                else North
                            else North
                        
                        let ship = { Coords = shipCoords; Center = center; Facing = direction; Name = name }
                        
                        // Vérifie si on doit exclure ce bateau
                        match excludeShip with
                        | Some(excludedShip) when ship.Name = excludedShip.Name -> acc
                        | _ -> ship :: acc
                    else acc
                | _ -> acc
            ) [] grid
        
        // Vérifie si la coordonnée est dans le périmètre d'un de ces bateaux
        List.exists (fun ship ->
            let perimeter = Ship.getPerimeter ship dims
            List.contains coord perimeter
        ) ships

    (* --- Nouvelles fonctions --- *)

    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let ship = Ship.createShip center direction name
        
        // Vérifie toutes les conditions
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau
            not (isCoordOccupied coord grid) &&
            // 3. Pas dans le périmètre d'un autre bateau
            not (isCoordInPerimeter coord None grid)
        ) ship.Coords

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let offset = Ship.getDirectionOffset direction
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        
        // Vérifie toutes les conditions pour les nouvelles coordonnées
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau
            not (isCoordOccupied coord grid) &&
            // 3. Pas dans le périmètre d'un autre bateau (excluant le bateau actuel)
            not (isCoordInPerimeter coord (Some ship) grid)
        ) newCoords

    let move (ship: Ship) (direction: Direction) : Ship =
        let offset = Ship.getDirectionOffset direction
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        let newCenter = Ship.addCoords ship.Center offset
        
        { ship with Coords = newCoords; Center = newCenter }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let newShip = Ship.createShip ship.Center direction ship.Name
        
        // Vérifie toutes les conditions pour les nouvelles coordonnées
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau
            not (isCoordOccupied coord grid) &&
            // 3. Pas dans le périmètre d'un autre bateau (excluant le bateau actuel)
            not (isCoordInPerimeter coord (Some ship) grid)
        ) newShip.Coords

    let rotate (ship: Ship) (direction: Direction) : Ship =
        Ship.createShip ship.Center direction ship.Name

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let offset = Ship.getDirectionOffset ship.Facing
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        
        // Pendant le jeu, ignore les périmètres - vérifie seulement les limites et occupations
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau
            not (isCoordOccupied coord grid)
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
        
        // Pendant le jeu, ignore les périmètres - vérifie seulement les limites et occupations
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau
            not (isCoordOccupied coord grid)
        ) finalCoords

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let newDirection = getNextDirection ship.Facing rotation
        let rotatedShip = Ship.createShip ship.Center newDirection ship.Name
        moveForward rotatedShip