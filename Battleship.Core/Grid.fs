namespace Battleship.Core

module Grid =

    type Dims = int * int
    type Coord = int * int
    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* Fonction helper pour List *)
    module ListHelpers =
        let rec foldi (f: int -> 'acc -> 'a -> 'acc) (acc: 'acc) (list: 'a list) : 'acc =
            let rec foldAux index acc items =
                match items with
                | [] -> acc
                | head :: tail -> 
                    let newAcc = f index acc head
                    foldAux (index + 1) newAcc tail
            foldAux 0 acc list

    (* Fonctions d'ordre supérieur pour manipuler la grille *)
    
    // Crée une grille avec des dimensions spécifiées, remplie avec une valeur par défaut
    let rec create (rows: int) (cols: int) (defaultValue: 'a) : 'a Grid =
        if rows <= 0 then Empty
        else 
            let row = List.replicate cols defaultValue
            Row(row, create (rows - 1) cols defaultValue)
    
    // Obtient un élément à une coordonnée spécifique
    let rec get (coord: Coord) (grid: 'a Grid) : 'a option =
        let (i, j) = coord
        match grid with
        | Empty -> None
        | Row(row, rest) ->
            if i = 0 then
                if j >= 0 && j < List.length row then
                    Some(List.item j row)
                else None
            else if i > 0 then
                get (i - 1, j) rest
            else None
    
    // Met à jour un élément à une coordonnée spécifique
    let rec set (coord: Coord) (value: 'a) (grid: 'a Grid) : 'a Grid =
        let (i, j) = coord
        match grid with
        | Empty -> Empty
        | Row(row, rest) ->
            if i = 0 then
                if j >= 0 && j < List.length row then
                    let newRow = List.mapi (fun idx item -> if idx = j then value else item) row
                    Row(newRow, rest)
                else Row(row, rest)
            else if i > 0 then
                Row(row, set (i - 1, j) value rest)
            else Row(row, rest)
    
    // Vérifie si une coordonnée est dans les limites de la grille
    let rec isInBounds (coord: Coord) (dims: Dims) : bool =
        let (i, j) = coord
        let (rows, cols) = dims
        i >= 0 && i < rows && j >= 0 && j < cols
    
    // Obtient les dimensions d'une grille
    let rec getDimensions (grid: 'a Grid) : Dims =
        let rec countRows g =
            match g with
            | Empty -> 0
            | Row(_, rest) -> 1 + countRows rest
        
        let getCols g =
            match g with
            | Empty -> 0
            | Row(row, _) -> List.length row
        
        (countRows grid, getCols grid)
    
    // Applique une fonction à chaque élément de la grille avec ses coordonnées
    let rec mapi (f: Coord -> 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        let rec mapRow rowIndex row =
            List.mapi (fun colIndex item -> f (rowIndex, colIndex) item) row
        
        let rec mapGrid rowIndex g =
            match g with
            | Empty -> Empty
            | Row(row, rest) ->
                let newRow = mapRow rowIndex row
                Row(newRow, mapGrid (rowIndex + 1) rest)
        
        mapGrid 0 grid
    
    // Applique une fonction à chaque élément de la grille
    let map (f: 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        mapi (fun _ item -> f item) grid
    
    // Collecte tous les éléments qui satisfont un prédicat avec leurs coordonnées
    let rec fold (f: 'acc -> Coord -> 'a -> 'acc) (acc: 'acc) (grid: 'a Grid) : 'acc =
        let rec foldRow rowIndex acc row =
            ListHelpers.foldi (fun colIndex acc item -> f acc (rowIndex, colIndex) item) acc row
        
        let rec foldGrid rowIndex acc g =
            match g with
            | Empty -> acc
            | Row(row, rest) ->
                let newAcc = foldRow rowIndex acc row
                foldGrid (rowIndex + 1) newAcc rest
        
        foldGrid 0 acc grid
    
    // Trouve tous les éléments qui satisfont un prédicat
    let filter (predicate: Coord -> 'a -> bool) (grid: 'a Grid) : (Coord * 'a) list =
        fold (fun acc coord item -> 
            if predicate coord item then (coord, item) :: acc 
            else acc) [] grid
        |> List.rev
    
    // Trouve le premier élément qui satisfait un prédicat
    let rec tryFind (predicate: Coord -> 'a -> bool) (grid: 'a Grid) : (Coord * 'a) option =
        let rec findInRow rowIndex row =
            let rec findInRowAux colIndex items =
                match items with
                | [] -> None
                | head :: tail ->
                    if predicate (rowIndex, colIndex) head then
                        Some((rowIndex, colIndex), head)
                    else findInRowAux (colIndex + 1) tail
            findInRowAux 0 row
        
        let rec findInGrid rowIndex g =
            match g with
            | Empty -> None
            | Row(row, rest) ->
                match findInRow rowIndex row with
                | Some result -> Some result
                | None -> findInGrid (rowIndex + 1) rest
        
        findInGrid 0 grid
    
    // Convertit une grille en liste de coordonnées et valeurs
    let toList (grid: 'a Grid) : (Coord * 'a) list =
        fold (fun acc coord item -> (coord, item) :: acc) [] grid
        |> List.rev