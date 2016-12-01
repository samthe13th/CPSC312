module Battleship where

  import Data.Char
  import System.Random
  import Data.List.Split

  blankGrid = ["~~X~~~~~~~","~X~~~~~~~X","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~"]
  computerStart = ["~0~~~~~~~~","~0~~~3~~~~","~0~~~~111~","~0~~~~~~~5","~~~44~~~~5","~~~~~~~2~~","~~~~~~~2~~","~~9~~~~2~~","~~~~~8~~~~","~66~~~~~7~"]
  playerStart = ["~~~~111~~~","~~~~~~~~6~","~3~7~~~~~~","~3~~~~44~~","~~~~~~~~~~","~~8~~~~~0~","55~~~~~~0~","~~~~9~2~0~","~~~~~~2~0~","~~~~~~2~~~"]
  title = "\n  ____    _  _____ _____ _     _____ ____  _   _ ___ ____ \n | __ )  / \\|_   _|_   _| |   | ____/ ___|| | | |_ _|  _ \\ \n |  _ \\ / _ \\ | |   | | | |   |  _| \\___ \\| |_| || || |_) | \n | |_) / ___ \\| |   | | | |___| |___ ___) |  _  || ||  __/ \n |____/_/   \\_\\_|   |_| |_____|_____|____/|_| |_|___|_| \n\n                    By Sam and Regina\n\n"
  abc = "   A B C D E F G H I J \n"
  divider = "   ___________________\n\n"

  start =
    do
      putStrLn title
      putStrLn "Enter your name:"
      p <- getLine
      putStrLn "Enter computer player's name:"
      c <- getLine
      putStrLn "PLACE YOUR SHIPS!"
      putStrLn (drawBoard blankGrid "human")
      setUp blankGrid p c 4 0

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
            let pShip = parseShip ship s
            let newGrid = (addShips pShip grid (head $ show acc))
            putStrLn (drawBoard newGrid "human")
            print acc
            setUp newGrid p c (if (acc < 2) then 3 else if (acc < 5) then 2 else 1) (acc + 1)

  parseShip (h:t) size =
    do
      let ship = (splitOn " " (h:t))
      if size == 1
        then down (ship !! 0) 1
        else if ((ship !! 1) == "down")
          then (down (ship !! 0) size)
          else (right (ship !! 0) size)

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

  newInput cGrid pGrid fb1 fb2 p c =
    do
      putStrLn "Please enter coordinates in the following form: 'A1'"
      line <- getLine
      if (checkInput line) then (continue cGrid pGrid fb1 fb2 p c line "random") else (newInput cGrid pGrid fb1 fb2 p c)

  continue cGrid pGrid fb1 fb2 p c line strategy =
    do
      let (a,b) = getCoordinates line
      let newGrid = attack cGrid (getCoordinates line) 0
      let fb = (hitOrMiss cGrid (a,b)) ++ (isSunk (getCoordinates line) cGrid newGrid "human" c) ++ (endGame newGrid "human" p)
      computerPlay newGrid pGrid fb p c strategy

  checkInput (h:t)
    | (elem h "ABCDEFGHIJabcdefghij") && (elem t ["1","2","3","4","5","6","7","8","9","10"]) = True
    | otherwise = False

{--
  strategyMove g (r,i)
    | marked g ((r + 1), i) = coinToss g ((r - 1), i) ((r + 2), i)
    | marked g ((r - 1), i) = coinToss g ((r + 1), i) ((r - 2), i)
    | marked g (r,(i + 1)) = coinToss g (r,(i - 1)) (r,(i + 2))
    | marked g (r,(i - 1)) = coinToss g (r,(i + 1)) (r,(i - 2))
    | otherwise = strategicRandom g (r,i) -}

  computerPlay cGrid pGrid fb p c strategy =
    do
      (r,i) <- (if strategy == "random" then (randomCoords pGrid) else (strategyCoords strategy pGrid))
      let fb1 = "Firing... " ++ fb
      let newGrid = (attack pGrid (r,i) 0)
      let fb2 = c ++ "'s move: " ++ (hitOrMiss pGrid (r,i)) ++ (isSunk (r,i) pGrid newGrid "computer" c) ++ (endGame newGrid "computer" c)
      newStrategy <- getStrategy strategy pGrid (r,i) newGrid
      personPlay cGrid newGrid fb1 fb2 p c newStrategy

  getStrategy strategy pGrid (r,i) ng
    | ((ng !! r) !! i) == 'X' && (isSunk (r,i) pGrid ng "computer" "any") == "" = return (show (r,i))
    | strategy == "random" || (isSunk (r,i) pGrid ng "computer" "any") /= "" = return "random"
    | otherwise = return strategy

  randomCoords g =
    do
      r <- randNum 9
      r2 <- randNum 9
      let i = fromIntegral r :: Int
      let i2 = fromIntegral r2 :: Int
      if (open (i,i2) g) then return (i,i2) else randomCoords g

  strategyCoords coords g =
    do
      let coord = read coords :: (Int,Int)
      newCoord <- guess coord g
      return newCoord

  guess (r,i) g =
    do
      random4 <- (randNum 3)
      let option = getOption (r,i) random4
      if (open option g) then return option else (guess (r,i) g)

  getOption (r,i) x
    | x == 0 = ((r + 1), i)
    | x == 1 = ((r - 1), i)
    | x == 2 = (r, (i + 1))
    | x == 3 = (r, (i - 1))

  open (r,i) g = if (legal (r,i) && not (elem ((g !! r) !! i) "X/")) then True else False

  legal (r,i) = if (r >= 0 && r <= 9 && i >= 0 && i <= 9) then True else False

  row grid 9 i = (show 10) ++ (renderRow (grid !! 9) i) ++ "\n"
  row grid n i = " " ++ (show (n + 1)) ++ (renderRow (grid !! n) i)++ "\n" ++ row grid (n + 1) i

  attack [] _ _ = []
  attack (h:t) (r,i) acc
    | acc == r = (updateRow h i 0):attack t (r,i) (acc + 1)
    | otherwise = h:attack t (r,i) (acc + 1)

  updateRow [] _ _ = []
  updateRow (h:t) i acc
    | i == acc = (collision h):updateRow t i (acc + 1)
    | otherwise = h:updateRow t i (acc + 1)

  collision x
    | elem x "0123456789X" = 'X'
    | otherwise = '/'

  renderRow [] _ = []
  renderRow (h:t) x = ' ':[renderElem h x] ++ (renderRow t x)
  renderElem e x
    -- for testing purposes:
--    | elem e "0123456789" = e
    | elem e "0123456789" = (if x == "computer" then '~' else 'O')
    | otherwise = e

  drawBoard g i = abc ++ (row g 0 i)

  hitOrMiss grid (r,i)
    | elem ((grid !! r) !! i) "0123456789X" = "HIT!"
    | otherwise = "MISS"

  getCoordinates (h:t) = ((digitToInt(convert h)),((read t :: Int) - 1))

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

  randNum n =
    do
      g <- newStdGen
      let (x,s) = randomR (0,n) g :: (Int,StdGen)
      return x

  isSunk (r,i) g0 g player c
    | elem ((g0 !! r) !! i) [e | v <- g, e <- v] = ""
    | otherwise = outputSunkText player c

  outputSunkText player c
    | player == "human" = " You sunk " ++ c ++ "'s battleship!"
    | otherwise = " " ++ c ++ " sunk your battleship!!"

  endGame g player name
    | [x | x <- [e | v <- g, e <- v], elem x "0123456789"] == "" =
      do
        "GAME OVER"
        if (player == "human") then "You win!!" else (name ++ " wins!")
    | otherwise = ""

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

  right start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1], y <- [y1..(y1 + (n - 1))]]

  down start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1..(x1 + (n - 1))], y <- [y1]]
