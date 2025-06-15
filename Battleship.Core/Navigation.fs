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
        
        // Collecte tous les noms de bateaux uniques (sauf celui exclu)
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
        
        // Pour chaque bateau, reconstruit ses coordonnées et calcule son périmètre
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
                // Crée un bateau temporaire pour calculer le périmètre
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
        
        // Vérifie seulement les conditions de base pour déboguer
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau (en excluant le bateau actuel)
            match Grid.get coord grid with
            | Some(Active(name, _)) -> name = ship.Name  // Permet si c'est le même bateau
            | _ -> true  // Clear ou None = OK
        ) newCoords

    let move (ship: Ship) (direction: Direction) : Ship =
        let offset = Ship.getDirectionOffset direction
        let newCoords = List.map (Ship.addCoords offset) ship.Coords
        let newCenter = Ship.addCoords ship.Center offset
        
        { ship with Coords = newCoords; Center = newCenter }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let dims = Grid.getDimensions grid
        let newShip = Ship.createShip ship.Center direction ship.Name
        
        // Vérifie seulement les conditions de base pour déboguer
        List.forall (fun coord ->
            // 1. Dans les limites de la grille
            Grid.isInBounds coord dims &&
            // 2. Pas occupé par un autre bateau (en excluant le bateau actuel)
            match Grid.get coord grid with
            | Some(Active(name, _)) -> name = ship.Name  // Permet si c'est le même bateau
            | _ -> true  // Clear ou None = OK
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