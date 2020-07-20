module LinGameOfLife

import Data.List
import Data.Vect
import Data.Strings
import System.Random
import System.Clock

%default total

snoc : (1 _ : Vect len a) -> (1 _ : a) -> Vect (S len) a
snoc [] x = [x]
snoc (y :: xs) x = y :: snoc xs x

iterateN : (n : Nat) -> (a -> a) -> a -> Vect n a
iterateN 0 f x = []
iterateN 1 f x = [x]
iterateN (S (S k)) f x = x :: iterateN (S k) f (f x)

toList : (1 _ : Vect n a) -> List a
toList [] = []
toList (x :: xs) = x :: toList xs

public export
record RingZipper n a where
  constructor MkRingZipper
  1 before : Vect n a
  1 focus : a
  1 after : Vect n a

export
Functor (RingZipper n) where
  map f (MkRingZipper before focus after)
    = MkRingZipper (map f before) (f focus) (map f after)

uncons : (1 _ : Vect (S n) a) -> (a, Vect n a)
uncons (x :: xs) = (x, xs)

unsnoc : (1 _ : Vect (S n) a) -> (Vect n a, a)
unsnoc [x] = ([], x)
unsnoc (x :: (y :: xs))
  = let (ys, y) = unsnoc (y :: xs) in (x :: ys, y)

export
rightShift : (1 _ : RingZipper (S n) a) -> RingZipper (S n) a
rightShift (MkRingZipper ls f rs) =
  let (lsInit, lsLast) = unsnoc ls
      (rsHead, rsTail) = uncons rs
      1 ls' = f :: lsInit
      1 rs' = snoc rsTail lsLast in
      MkRingZipper ls' rsHead rs'

export
leftShift : (1 _ : RingZipper (S n) a) -> RingZipper (S n) a
leftShift (MkRingZipper ls f rs) =
  let (lsHead, lsTail) = uncons ls
      (rsInit, rsLast) = unsnoc rs
      1 ls' = snoc lsTail rsLast
      1 rs' = f :: rsInit in
      MkRingZipper ls' lsHead rs'

export
extract : RingZipper n a -> a
extract z = z.focus

export
duplicate : {n : Nat} -> RingZipper (S n) a -> RingZipper (S n) (RingZipper (S n) a)
duplicate z = let ls = reverse $ iterateN (S n) leftShift (leftShift z)
                  rs = iterateN (S n) rightShift (rightShift z) in
                  MkRingZipper ls z rs

export
extend : {n : Nat} -> (RingZipper (S n) a -> b) -> RingZipper (S n) a -> RingZipper (S n) b
extend f = map f . duplicate

public export
data Game : Nat -> Type -> Type where
  MkGame : RingZipper n (RingZipper n a) -> Game n a

export
Functor (Game n) where
  map f (MkGame g) = MkGame (map (map f) g)

namespace Game
  export
  leftShift : (1 _ : Game (S n) a) -> Game (S n) a
  leftShift (MkGame g) = MkGame (map leftShift g)

  export
  rightShift : (1 _ : Game (S n) a) -> Game (S n) a
  rightShift (MkGame g) = MkGame (map rightShift g)

  export
  upShift : (1 _ : Game (S n) a) -> Game (S n) a
  upShift (MkGame g) = MkGame (leftShift g)

  export
  downShift : (1 _ : Game (S n) a) -> Game (S n) a
  downShift (MkGame g) = MkGame (rightShift g)

  export
  extract : Game n a -> a
  extract (MkGame g) = extract $ extract g

  export
  duplicate : {n : Nat} -> Game (S n) a -> Game (S n) (Game (S n) a)
  duplicate {n} g = let il = iterateN (S n) leftShift (leftShift g)
                        ir = iterateN (S n) rightShift (rightShift g)
                        inner = MkRingZipper il g ir
                        ol = iterateN (S n) (map upShift) (map upShift inner)
                        or = iterateN (S n) (map downShift) (map downShift inner) in
                        MkGame (MkRingZipper ol inner or)

  export
  extend : {n : Nat} -> (Game (S n) a -> b) -> Game (S n) a -> Game (S n) b
  extend f = map f . duplicate

public export
data Cell = Alive | Dead

export
Show Cell where
  show Alive = "X"
  show Dead = " "

export
isAlive : (1 _ : Cell) -> Bool
isAlive Alive = True
isAlive Dead = False

export
isDead : (1 _ : Cell) -> Bool
isDead Alive = False
isDead Dead = True

aliveNeighbours : Game (S n) Cell -> Nat
aliveNeighbours g = let br = extract . upShift . leftShift $ g
                        bc = extract . upShift $ g
                        bl = extract . upShift . rightShift $ g
                        cr = extract . leftShift $ g
                        cl = extract . rightShift $ g
                        tr = extract . downShift . leftShift $ g
                        tc = extract . downShift $ g
                        tl = extract . downShift . rightShift $ g in
                        sum $ map toNat $ the (List Cell) [tl, tc, tr, cl, cr, bl, bc, br]
  where toNat : (1 _ : Cell) -> Nat
        toNat Alive = 1
        toNat Dead = 0

stepCell : Game (S n) Cell -> Cell
stepCell g = let cell = extract g
                 ns = aliveNeighbours g in
                 if ns > 3 || ns < 2 then Dead
                    else if isDead cell && ns == 3 then Alive
                            else cell

export
nextStep : {n : Nat} -> Game (S n) Cell -> Game (S n) Cell
nextStep = extend stepCell

-- export
-- example : Game 8 Cell
-- example = let deadRow = MkRingZipper (replicate 8 Dead) Dead (replicate 8 Dead)
--               topRow = MkRingZipper (replicate 8 Dead) Alive (replicate 8 Dead)
--               centerRow = MkRingZipper (replicate 8 Dead) Dead (Alive :: replicate 7 Dead)
--               bottomRow = MkRingZipper (Alive :: replicate 7 Dead) Alive (Alive :: replicate 7 Dead) in
--               MkGame (MkRingZipper (topRow :: replicate 7 deadRow) centerRow (bottomRow :: replicate 7 deadRow))

append : (1 _ : List a) -> (1 _ : List a) -> List a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

gameLine : {n : Nat} -> Nat -> (1 _ : RingZipper n Cell) -> List (Nat, Nat, Cell)
gameLine r (MkRingZipper before focus after) =
  let 1 b = LinGameOfLife.toList $ reverse $ zip3 (replicate n r) (iterateN n (`minus` 1) (n `minus` 1)) before
      1 a = LinGameOfLife.toList $ zip3 (replicate n r) (iterateN n S (n + 1)) after in
      append b ((r, n, focus) :: a)

concat : (1 _ : List (List a)) -> List a
concat [] = []
concat (x :: xs) = append x (concat xs)

export
gameSquare : {n : Nat} -> (1 _ : Game n Cell) -> List (Nat, Nat, Cell)
gameSquare (MkGame (MkRingZipper before focus after)) =
  let 1 b = LinGameOfLife.toList $ reverse $ zipWith (\i, r => gameLine i r) (iterateN n (`minus` 1) (n `minus` 1)) before
      1 a = LinGameOfLife.toList $ zipWith (\i, r => gameLine i r) (iterateN n S (n + 1)) after in
      concat $ append b ((gameLine n focus) :: a)

export
randomGame : {radius : Nat} -> IO (Game (S radius) Cell)
randomGame {radius} = do
    before <- traverse (const randomZipper) $ replicate (S radius) ()
    focus <- randomZipper
    after <- traverse (const randomZipper) $ replicate (S radius) ()
    pure $ MkGame (MkRingZipper before focus after)
  where
    fromInt : Int -> Cell
    fromInt x = if x < 0 then Dead else Alive
    randomZipper : {n : Nat} -> IO (RingZipper n Cell)
    randomZipper {n} = do
      before <- traverse (const $ fromInt <$> randomIO) $ replicate n ()
      focus <- fromInt <$> randomIO
      after <- traverse (const $ fromInt <$> randomIO) $ replicate n ()
      pure $ MkRingZipper before focus after

testPerf : IO ()
testPerf = do
    game <- randomGame {radius = 14}
    times <- loop 20 game
    let avg = (sum times `div` cast (length times)) `div` 1000000
    putStrLn $ "Average: " ++ show avg ++ " ms"
  where
    loop : {radius : Nat} -> Nat -> Game (S radius) Cell -> IO (List Integer)
    loop 0 game = pure []
    loop (S n) game = do
      start <- clockTime Monotonic
      let game' = nextStep game
      end <- clockTime Monotonic
      let delta = timeDifference end start
      putStrLn $ "Time: " ++ show (nanoseconds delta `div` 1000000) ++ " ms"
      rest <- loop n game'
      pure $ nanoseconds delta :: rest
