module Battleship where

  import Data.Char
  import System.Random
  import Data.List.Split

  blankGrid = ["~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~"]
  computerStart = ["~0~~~~~~~~","~0~~~3~~~~","~0~~~~111~","~0~~~~~~~5","~~~44~~~~5","~~~~~~~2~~","~~~~~~~2~~","~~9~~~~2~~","~~~~~8~~~~","~66~~~~~7~"]
  playerStart = ["~~~~111~~~","~~~~~~~~6~","~3~7~~~~~~","~3~~~~44~~","~~~~~~~~~~","~~8~~~~~0~","55~~~~~~0~","~~~~9~2~0~","~~~~~~2~0~","~~~~~~2~~~"]
  title = "\n  ____    _  _____ _____ _     _____ ____  _   _ ___ ____ \n | __ )  / \\|_   _|_   _| |   | ____/ ___|| | | |_ _|  _ \\ \n |  _ \\ / _ \\ | |   | | | |   |  _| \\___ \\| |_| || || |_) | \n | |_) / ___ \\| |   | | | |___| |___ ___) |  _  || ||  __/ \n |____/_/   \\_\\_|   |_| |_____|_____|____/|_| |_|___|_| \n\n                    By Sam and Regina\n\n"
  abc = "   A B C D E F G H I J \n"
  divider = "   --------------------\n"
  -- start a new game
  start =
    do
      putStrLn title
      putStrLn "Enter your name:"
      p <- getLine
      putStrLn "Enter computer player's name:"
      c <- getLine
      putStrLn "\nPLACE YOUR SHIPS!"
      putStrLn (drawBoard blankGrid "human")
      setUp blankGrid p c 4 0

  {--
   places a "ship" on the player's board given an input of a coordinate and direction, eg 'A1 down'
   function called recursively until accumlulator (acc) reaches 10.
   p and c are names of the human and computer playerStart
   s is the size of the ship being places (1-4 squares)
   -}
  setUp grid p c s 10 =
    do
      print "done set up"
      personPlay computerStart grid "" "" p c "random"
  setUp grid p c s acc =
    do
      putStrLn ("Place ship: SIZE " ++ (show s) ++ ".")
      putStrLn "Enter starting coordinate and direction (right or down)"
      putStrLn "eg 'a1 down'"
      putStrLn "OR enter 'auto' to automatically place ships"
      ship <- getLine
      if (ship == "auto")
        then
          do
            putStrLn "Auto set-up"
            personPlay computerStart playerStart "" "" p c "random"
        else
          do
            validation <- validateShip ship s grid
            if (validation)
              then
                do
                  let pShip = parseShip ship s
                  let newGrid = (addShips pShip grid (head $ show acc))
                  putStrLn (drawBoard newGrid "human")
                  setUp newGrid p c (if (acc < 2) then 3 else if (acc < 5) then 2 else 1) (acc + 1)
              else
                do
                  putStrLn (drawBoard grid "human")
                  putStrLn "ENTRY NOT VALID \n"
                  setUp grid p c s acc

  {--
      Checks to see if a new ship can be placed on board.
      Returns False if:
        - Input text is not two words long
        - Input text is not a coordinate followed by "down" or "right"
        - Ship does not fit on board
        - Ship overlaps with a ship already on board
      Otherwise returns True
  -}
  validateShip ship size grid =
    do
      let shipList = splitOn " " ship
      if (length shipList /= 2) then (return False)
      else if (not (checkInput (shipList !! 0) && elem (shipList !! 1) ["down","right"]))
        then (return False)
      else if (not (checkShipPlaces (parseShip ship size) grid))
        then (return False)
        else (return True)

  {--
    Given a ship placement text, places a ship on the board
  -}
  parseShip (h:t) size =
    do
      let ship = (splitOn " " (h:t))
      if size == 1
        then down (ship !! 0) 1
        else if ((ship !! 1) == "down")
          then (down (ship !! 0) size)
          else (right (ship !! 0) size)

  {-- takes:
        - computer grid state (cGrid)
        - the player grid state (pGrid)
        - text feedback for the computer and player moves (fb1, fb2)
        - names of the player and computer (p,c)
        - the "strategy" of the computer (st)
      User input: position to fire on
        - if validated, calls "continue"
        - if not, calls "newInput"
      -}
  personPlay cGrid pGrid fb1 fb2 p c st =
    do
      putStrLn ((drawBoard cGrid "computer") ++ divider ++ (drawBoard pGrid "human"))
      putStrLn fb1
      putStrLn fb2
      putStrLn ("\n" ++ p ++ "'s move: Enter coordinates to fire on! (eg 'A1')")
      line <- getLine
      if (checkInput line)
        then (continue cGrid pGrid fb1 fb2 p c line st)
        else (newInput cGrid pGrid fb1 fb2 p c)

  {-- gets new input from user for firing on coordinate-}
  newInput cGrid pGrid fb1 fb2 p c =
    do
      putStrLn "Please enter coordinates in the following form: 'A1'"
      line <- getLine
      if (checkInput line) then (continue cGrid pGrid fb1 fb2 p c line "random") else (newInput cGrid pGrid fb1 fb2 p c)

  {-- updates computer grid state (cGrid) with square that is fired on, then calls computerPlay with updated state -}
  continue cGrid pGrid fb1 fb2 p c line strategy =
    do
      let (a,b) = getCoordinates line
      let newGrid = attack cGrid (getCoordinates line) 0
      let fb = (hitOrMiss cGrid (a,b)) ++ (isSunk (getCoordinates line) cGrid newGrid "human" c) ++ (endGame newGrid "human" p)
      computerPlay newGrid pGrid fb p c strategy

  {-- checks if coordinate is a legal coordinate (eg "A1")-}
  checkInput (h:t)
    | (elem h "ABCDEFGHIJabcdefghij") && (elem t ["1","2","3","4","5","6","7","8","9","10"]) = True
    | otherwise = False

    {-- takes:
          - computer grid state (cGrid)
          - the player grid state (pGrid)
          - text feedback for the computer and player moves (fb1, fb2)
          - names of the player and computer (p,c)
          - the "strategy" of the computer (st)
        Fires on player grid based on "strategy", then updates strategy and player Grid,
         and then calls personPlay
        -}
  computerPlay cGrid pGrid fb p c strategy =
    do
      (r,i) <- (if strategy == "random" then (randomCoords pGrid) else (strategyCoords strategy pGrid))
      let fb1 = "Firing... " ++ fb
      let newGrid = (attack pGrid (r,i) 0)
      let fb2 = c ++ "'s move: " ++ (hitOrMiss pGrid (r,i)) ++ (isSunk (r,i) pGrid newGrid "computer" c) ++ (endGame newGrid "computer" c)
      newStrategy <- getStrategy strategy pGrid (r,i) newGrid
      personPlay cGrid newGrid fb1 fb2 p c newStrategy

  {-- gets a strategy for picking the next square to fire on. A ship has been hit but not sunk
  Computer picks another square next to the one hit. If two squares are hit next to each other,
  the computer picks one the squares on either end of the the two hit. Otherwise, the computer fires
  on a random square -}
  getStrategy strategy pGrid (r,i) ng
    | ((ng !! r) !! i) == 'X' && (isSunk (r,i) pGrid ng "computer" "any") == "" = return (show (r,i))
    | strategy == "random" || (isSunk (r,i) pGrid ng "computer" "any") /= "" = return "random"
    | otherwise = return strategy

  {-- returns random coordinates that have NOT been fired on before (based on grid, g)-}
  randomCoords g =
    do
      r <- randNum 9
      r2 <- randNum 9
      let i = fromIntegral r :: Int
      let i2 = fromIntegral r2 :: Int
      if (open (i,i2) g) then return (i,i2) else randomCoords g

  {-- returns coordinates to fire on generated from computer strategy -}
  strategyCoords coords g =
    do
      let coord = read coords :: (Int,Int)
      newCoord <- smartMove coord g
      return newCoord

  {-- returns coordinates based on where a ship part is likely to be (if 2 hit blocks are next to eachother)-}
  smartMove (r,i) g
    | hit ((r + 1),i) g && open ((r - 1),i) g = return ((r - 1),i)
    | hit ((r + 1),i) g && open ((r + 2),i) g = return ((r + 2),i)
    | hit ((r - 1),i) g && open ((r + 1),i) g = return ((r + 1),i)
    | hit ((r - 1),i) g && open ((r - 2),i) g = return ((r - 2),i)
    | hit (r,(i + 1)) g && open (r,(i - 1)) g = return (r,(i - 1))
    | hit (r,(i + 1)) g && open (r,(i + 2)) g = return (r,(i + 2))
    | hit (r,(i - 1)) g && open (r,(i + 1)) g = return (r,(i + 1))
    | hit (r,(i - 1)) g && open (r,(i - 2)) g = return (r,(i - 2))
    | otherwise = guess4 (r,i) g

  {-- returns random coordinates for squares directly touching a hit square -}
  guess4 (r,i) g =
    do
      random4 <- (randNum 3)
      let option = getOption (r,i) random4
      if (open option g) then return option else (guess4 (r,i) g)

  {-- evals to a coordinate given randomly generated number between 1 and 4 -}
  getOption (r,i) x
    | x == 0 = ((r + 1), i)
    | x == 1 = ((r - 1), i)
    | x == 2 = (r, (i + 1))
    | x == 3 = (r, (i - 1))

  {-- evals to True if given coordinates (r,i) on a grid, g, are part of a ship that is hit -}
  hit (r,i) g = if ((legal (r,i)) && ((g !! r) !! i) == 'X') then True else False

  {-- evals to True if given coordinates (r,i), on a grid, g, have not been fired on -}
  open (r,i) g = if (legal (r,i) && not (elem ((g !! r) !! i) "X/")) then True else False

  {-- evals to True if given coordinates (r,i), on a grid, g, are open water -}
  water (r,i) g = if ((legal (r,i)) && ((g !! r) !! i) == '~') then True else False

  {-- evals to True if given coordinates (r,i) are legal for a 10 by 10 grid, g -}
  legal (r,i) = if (r >= 0 && r <= 9 && i >= 0 && i <= 9) then True else False

  {-- evals to a string representing a grid of rows of cells -}
  renderGrid grid 9 i = (show 10) ++ (renderRow (grid !! 9) i) ++ "\n"
  renderGrid grid n i = " " ++ (show (n + 1)) ++ (renderRow (grid !! n) i)++ "\n" ++ renderGrid grid (n + 1) i

  {-- recursive function that finds and changes a cell in a grid being fired on -}
  attack [] _ _ = []
  attack (h:t) (r,i) acc
    | acc == r = (updateRow h i 0):attack t (r,i) (acc + 1)
    | otherwise = h:attack t (r,i) (acc + 1)
  {-- updates a row in a grid containing a cell being attacked -}

  updateRow [] _ _ = []
  updateRow (h:t) i acc
    | i == acc = (collision h):updateRow t i (acc + 1)
    | otherwise = h:updateRow t i (acc + 1)
  {-- if cell contains part of a ship, mark as hit ('X') -}

  collision x
    | elem x "0123456789X" = 'X'
    | otherwise = '/'

  {-- renders a row of cells as a string to display -}
  renderRow [] _ = []
  renderRow (h:t) x = ' ':[renderElem h x] ++ (renderRow t x)
  renderElem e x
    | elem e "0123456789" = (if x == "computer" then '~' else 'O')
    | otherwise = e

  {-- evals to a string representing a grid with leters along the top (A-J)-}
  drawBoard g i = "\n" ++ abc ++ (renderGrid g 0 i)

  {-- evals to "HIT!" if given coordinates represent part of a ship, else eval to "MISS"-}
  hitOrMiss grid (r,i)
    | elem ((grid !! r) !! i) "0123456789X" = "HIT!"
    | otherwise = "MISS"

  {-- convert string of coordinates eg "(0,0)" to tuple eg (0,0) -}
  getCoordinates (h:t) = (((read t :: Int) - 1),(digitToInt(convert h)))

  {-- convert letter character to coordinate character-}
  convert x
    | (toUpper x) == 'A' = '0'
    | (toUpper x) == 'B' = '1'
    | (toUpper x)  == 'C' = '2'
    | (toUpper x)  == 'D' = '3'
    | (toUpper x)  == 'E' = '4'
    | (toUpper x)  == 'F' = '5'
    | (toUpper x)  == 'G' = '6'
    | (toUpper x)  == 'H' = '7'
    | (toUpper x)  == 'I' = '8'
    | (toUpper x)  == 'J' = '9'

  {-- returns random number in range 0 to a given number, n -}
  randNum n =
    do
      g <- newStdGen
      let (x,s) = randomR (0,n) g :: (Int,StdGen)
      return x

  {-- evals to blank string if coordinate on a sunk ship, else evals as feedback-}
  isSunk (r,i) g0 g player c
    | elem ((g0 !! r) !! i) [e | v <- g, e <- v] = ""
    | otherwise = outputSunkText player c

  {-- evals to text feedback if a ship is sunk-}
  outputSunkText player c
    | player == "human" = " You sunk " ++ c ++ "'s battleship!"
    | otherwise = " " ++ c ++ " sunk your battleship!!"

  {-- evals to endgame output text -}
  endGame g player name
    | [x | x <- [e | v <- g, e <- v], elem x "0123456789"] == "" =
      do
        "GAME OVER"
        if (player == "human") then "You win!!" else (name ++ " wins!")
    | otherwise = ""

  {-- Add list of ships to grid -}
  addShips [] grid n = grid
  addShips (h:t) grid n = addShips t (addShip grid h 0 n) n
  addShip [] _ _ n = []
  addShip (h:t) (r,i) acc n
    | r == acc = (addShipPart h i 0 n):addShip t (r,i) (acc + 1) n
    | otherwise = h:(addShip t (r,i) (acc + 1)) n
  addShipPart [] _ _ n = []
  addShipPart (h:t) i acc n
    | i == acc = n:addShipPart t i (acc + 1) n
    | otherwise = h:addShipPart t i (acc + 1) n

  {-- eval to True if ship can be placed on 10 by 10 grid legally -}
  checkShipPlaces (h:t) grid = foldr (&&) True (checkShipPlace (h:t) grid)
  checkShipPlace [] _ = []
  checkShipPlace (h:t) grid = (water h grid : checkShipPlace t grid)

 {-- evals to list of coordinates for describing a ship of size 'n' pointing to the right -}
  right start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1], y <- [y1..(y1 + (n - 1))]]

  {-- evals to list of coordinates for describing a ship of size 'n' pointing to the right -}
  down start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1..(x1 + (n - 1))], y <- [y1]]
