import Data.List
import System.IO
import System
--import Data.Array.IO
import Array
import List
import Data.Map(keys, elems)

--indesado base 0
getXRow table x = table !! x 


-- indesado base 0
getXColumn (x:[]) y = [x !! y]
getXColumn (x:xs) y = [x !! y] ++ getXColumn xs y

getQuadrant1 table = take 3 (table !! 0) ++ take 3 (table !! 1) ++ take 3 (table !! 2)
getQuadrant2 table = take 3 (drop 3 (table !! 0)) ++ take 3 (drop 3 (table !! 1)) ++ take 3 (drop 3 (table !! 2))
getQuadrant3 table = take 3 (reverse (table !! 0)) ++ take 3 (reverse (table !! 1)) ++ take 3 ( reverse (table !! 2) )
getQuadrant4 table = take 3 (table !! 3) ++ take 3 (table !! 4) ++ take 3 (table !! 5)
getQuadrant5 table = take 3 (drop 3 (table !! 3)) ++ take 3 (drop 3 (table !! 4)) ++ take 3 (drop 3 (table !! 5))
getQuadrant6 table = take 3 (reverse (table !! 3)) ++ take 3 (reverse (table !! 4)) ++ take 3 (reverse (table !! 5))
getQuadrant7 table = take 3 (table !! 6) ++ take 3 (table !! 7) ++ take 3 (table !! 8)
getQuadrant8 table = take 3 (drop 3 (table !! 6)) ++ take 3 (drop 3 (table !! 7)) ++ take 3 (drop 3 (table !! 8))
getQuadrant9 table = take 3 (reverse (table !! 6)) ++ take 3 (reverse (table !! 7)) ++ take 3 (reverse (table !! 8))

getQuadrant table x = case x of  1 ->  getQuadrant1 table
                                 2 ->  getQuadrant2 table
                                 3 ->  getQuadrant3 table
                                 4 ->  getQuadrant4 table
                                 5 ->  getQuadrant5 table
                                 6 ->  getQuadrant6 table
                                 7 ->  getQuadrant7 table
                                 8 ->  getQuadrant8 table
                                 _ ->  getQuadrant9 table 

-- indesado base 0
getQuadrantNumber x y = if x < 3 then if y < 3 then 1 else if y < 6 then 2 else 3
                        else if x < 6 then if y < 3 then 4 else if y < 6 then 5 else 6
                        else if y < 3 then 7 else if y < 6 then 8 else 9

--indesado base 0
replaceX row x element = take (fromIntegral x) row ++ [ (fromIntegral element)] ++ drop ((fromIntegral x) + 1)  row 

--indesado base 0
replaceXY table x y element = take (fromIntegral x) table ++  [ (replaceX (table !! (fromIntegral x) ) y element) ] ++ drop ((fromIntegral x) + 1)  table


posibleNumbers li = filter (\x -> notElem x li) [1,2,3,4,5,6,7,8,9]

posibleNumbers2List li li1 =  filter (\x -> elem x li ) li1

--indesado base 0
--los posibles valores para una casilla x y en un tablero
posibleNumbersXY table x y = posibleNumbers2List (posibleNumbers2List (posibleNumbers (getXRow table x)) (posibleNumbers (getXColumn table y) )) (posibleNumbers (getQuadrant table (getQuadrantNumber x y))  )


removeNullBoards tables = filter (\x -> notElem x [[]]) tables


--generar nuevos tableros
generateNewBoards table x y = generateNewBoardsWithList table (posibleNumbersXY table x y) x y


--generar los uevos tableros con una lista de posibles valores para una casilla
generateNewBoardsWithList table [] x y = []
generateNewBoardsWithList table (xs:[]) x y = [replaceXY table x y xs]
generateNewBoardsWithList table (xs:teil) x y = [replaceXY table x y xs] ++ generateNewBoardsWithList table teil x y


findNullCellsRow (xs:[]) x y = if xs == 0 then [( (fromIntegral x) , (fromIntegral y) )] else []
findNullCellsRow (xs:teil) x y = (if xs == 0 then [( (fromIntegral x) , (fromIntegral y) )] else []) ++ findNullCellsRow teil x ((fromIntegral y) + 1)

findNullCellsXY1 (xs:[]) x y = findNullCellsRow xs x y
findNullCellsXY1 (xs:teil) x y = (findNullCellsRow xs x y) ++ findNullCellsXY1 teil ((fromIntegral x) + 1) 0

--encontrar casillas en 0
findNullCellsXY table = findNullCellsXY1 table 0 0 


minimun (xs:[]) = xs
minimun (xs:teil) = 
    let a = minimun teil
    in if xs !! 0 < a !! 0 then xs else a  


play1 lTable = 
    
    if length(lTable) == 0 then  []
    else if length(lTable) == 1 then
        let nullCells = findNullCellsXY (head lTable)
        in if length(nullCells) == 0 then [(head lTable)] else play1 (removeNullBoards( generateNewBoards (head lTable) (fst (head nullCells)) (snd (head nullCells) ) ))
    else
        let result = play1 [(head lTable)] in
        if length(result) == 0 then play1 (tail lTable) else result


getMinimunNullCell table []= [[]] 
getMinimunNullCell table (xs:[])= [[ length(posibleNumbersXY table (fst xs) (snd xs)) , (fst xs) , (snd xs) ]]
getMinimunNullCell table (xs:teil) = [minimun( [[length(posibleNumbersXY table (fst xs) (snd xs)) , (fst xs) , (snd xs) ]] ++ getMinimunNullCell table teil )]
    

play2 lTable = 
    
    if length(lTable) == 0 then  []
    else if length(lTable) == 1 then
        let nullCells = findNullCellsXY (head lTable)
        in if length(nullCells) == 0 then [(head lTable)] 
           else 
            let minCell = getMinimunNullCell (head lTable) nullCells 
            in play2 (removeNullBoards( generateNewBoards (head lTable) ((head minCell) !! 1) ((head minCell) !! 2 ) ))
    else
        let result = play2 [(head lTable)] in
        if length(result) == 0 then play2 (tail lTable) else result





