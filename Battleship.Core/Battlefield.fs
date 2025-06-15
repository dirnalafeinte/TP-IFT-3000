namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }
    
    // Trouve la direction d'un bateau à partir de ses coordonnées
    let findShipDirection (coords: Coord list) : Direction =
        if List.length coords <= 1 then North
        else
            let first = List.head coords
            let second = List.item 1 coords
            let (di, dj) = Ship.addCoords second (Ship.multiplyCoord first (-1))
            if di = 0 && dj = 1 then East
            elif di = 0 && dj = -1 then West
            elif di = 1 && dj = 0 then South
            else North
    
    // Reconstruit un bateau depuis ses secteurs dans la grille
    let reconstructShip (name: Name) (grid: Sector Grid) : Ship option =
        // Collecte toutes les coordonnées du bateau avec leurs indices
        let shipSectors = 
            Grid.fold (fun acc coord sector ->
                match sector with
                | Active(shipName, index) when shipName = name -> (coord, index) :: acc
                | _ -> acc
            ) [] grid
        
        if List.isEmpty shipSectors then None
        else
            // Trie par index pour avoir l'ordre correct
            let sortedCoords = 
                shipSectors
                |> List.sortBy snd
                |> List.map fst
            
            let size = List.length sortedCoords
            let centerIndex = Ship.getCenterIndex size
            let center = List.item centerIndex sortedCoords
            let direction = findShipDirection sortedCoords
            
            Some { Coords = sortedCoords; Center = center; Facing = direction; Name = name }

    (* --- Nouvelles fonctions --- *)

    let initClearGrid (dims: Dims) : Sector Grid =
        let (rows, cols) = dims
        Grid.create rows cols Clear

    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        // Ajoute chaque coordonnée du bateau à la grille avec son index
        let rec addShipCoords coords index currentGrid =
            match coords with
            | [] -> currentGrid
            | coord :: rest ->
                let newGrid = Grid.set coord (Active(ship.Name, index)) currentGrid
                addShipCoords rest (index + 1) newGrid
        
        addShipCoords ship.Coords 0 grid

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        // 1. Supprime l'ancien bateau de la grille
        let clearedGrid = 
            Grid.map (fun sector ->
                match sector with
                | Active(name, _) when name = ship.Name -> Clear
                | other -> other
            ) grid
        
        // 2. Ajoute le nouveau bateau
        addShip ship clearedGrid

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        match Grid.get coord grid with
        | Some(Active(name, _)) -> Some name
        | _ -> None

    let extractData (grid: Sector Grid) : Data =
        let dims = Grid.getDimensions grid
        
        // Trouve tous les noms de bateaux uniques dans la grille
        let shipNames = 
            Grid.fold (fun acc coord sector ->
                match sector with
                | Active(name, _) -> 
                    if List.contains name acc then acc
                    else name :: acc
                | _ -> acc
            ) [] grid
        
        // Reconstruit chaque bateau
        let ships = 
            shipNames
            |> List.choose (fun name -> reconstructShip name grid)
        
        { Dims = dims; Ships = ships }

    let loadData (data: Data) : Sector Grid =
        let grid = initClearGrid data.Dims
        
        // Ajoute chaque bateau à la grille
        let rec addShips ships currentGrid =
            match ships with
            | [] -> currentGrid
            | ship :: rest ->
                let newGrid = addShip ship currentGrid
                addShips rest newGrid
        
        addShips data.Ships grid