module Battleship where

  import Data.Char
  import System.Random
  import Data.List.Split

  blankGrid = ["~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~","~~~~~~~~~~"]
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
      personPlay computerStart grid "" "" p c

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
            personPlay computerStart playerStart "" "" p c
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

  personPlay cGrid pGrid fb1 fb2 p c =
    do
      putStrLn ((drawBoard cGrid "computer") ++ divider ++ (drawBoard pGrid "human"))
      putStrLn fb1
      putStrLn fb2
      putStrLn ("\n" ++ p ++ "'s move: Enter coordinates to fire on! (eg 'A1')")
      line <- getLine
      if (checkInput line)
        then (continue cGrid pGrid fb1 fb2 p c line)
        else (newInput cGrid pGrid fb1 fb2 p c)

  newInput cGrid pGrid fb1 fb2 p c =
    do
      putStrLn "Please enter coordinates in the following form: 'A1'"
      line <- getLine
      if (checkInput line) then (continue cGrid pGrid fb1 fb2 p c line) else (newInput cGrid pGrid fb1 fb2 p c)

  continue cGrid pGrid fb1 fb2 p c line =
    do
      let (a,b) = getCoordinates line
      let newGrid = attack cGrid (getCoordinates line) 0
      let fb = (hitOrMiss cGrid (a,b)) ++ (isSunk (getCoordinates line) cGrid newGrid "human" c) ++ (endGame newGrid "human" p)
      computerPlay newGrid pGrid fb p c

  checkInput (h:t)
    | (elem h "ABCDEFGHIJabcdefghij") && (elem t ["1","2","3","4","5","6","7","8","9","10"]) = True
    | otherwise = False

  computerPlay cGrid pGrid fb p c =
    do
      r <- randNum
      r2 <- randNum
      let i = fromIntegral r :: Int
      let i2 = fromIntegral r2 :: Int
      let fb1 = "Firing... " ++ fb
      let newGrid = (attack pGrid (i,i2) 0)
      let fb2 = c ++ "'s move: " ++ (hitOrMiss pGrid (i,i2)) ++ (isSunk (i,i2) pGrid newGrid "computer" c) ++ (endGame newGrid "computer" c)
      personPlay cGrid newGrid fb1 fb2 p c

  row grid 9 i = (show 10) ++ (renderRow (grid !! 9) i) ++ "\n"
  row grid n i = " " ++ (show (n + 1)) ++ (renderRow (grid !! n) i)++ "\n" ++ row grid (n + 1) i

  attack [] _ _ = []
  attack (h:t) (i,r) acc
    | acc == r = (updateRow h i 0):attack t (i,r) (acc + 1)
    | otherwise = h:attack t (i,r) (acc + 1)

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

  hitOrMiss grid (i,r)
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

  randNum =
    do
      g <- newStdGen
      let (x,s) = randomR (0,9) g :: (Int,StdGen)
      return x

  isSunk (i,r) g0 g player c
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
  addShip (h:t) (i,r) acc n
    | r == acc = (addShipPart h i 0 n):addShip t (i,r) (acc + 1) n
    | otherwise = h:(addShip t (i,r) (acc + 1)) n

  addShipPart [] _ _ n = []
  addShipPart (h:t) i acc n
    | i == acc = n:addShipPart t i (acc + 1) n
    | otherwise = h:addShipPart t i (acc + 1) n

  down start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1], y <- [y1..(y1 + (n - 1))]]

  right start n =
    do
      let (x1,y1) = getCoordinates start
      [(x,y) | x <- [x1..(x1 + (n - 1))], y <- [y1]]
