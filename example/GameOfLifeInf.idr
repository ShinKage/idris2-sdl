module GameOfLife

import Control.Comonad
import Data.List
import Data.Stream
import Data.Strings

%default total

export
fromList : List a -> Stream a -> Stream a
fromList [] s = s
fromList (x :: xs) s = x :: fromList xs s

public export
record Zipper a where
  constructor MkZipper
  before : Stream a
  focus : a
  after : Stream a

export
Functor Zipper where
  map f (MkZipper before focus after)
    = MkZipper (map f before) (f focus) (map f after)

export
leftShift : Zipper a -> Zipper a
leftShift (MkZipper ls f (r :: rs)) = MkZipper (f :: ls) r rs

export
rightShift : Zipper a -> Zipper a
rightShift (MkZipper (l :: ls) f rs) = MkZipper ls l (f :: rs)

export
Comonad Zipper where
  extract z = z.focus
  duplicate z = let ls = iterate leftShift (leftShift z)
                    rs = iterate rightShift (rightShift z) in
                    MkZipper ls z rs

public export
data Game : Type -> Type where
  MkGame : Zipper (Zipper a) -> Game a

export
Functor Game where
  map f (MkGame g) = MkGame (map (map f) g)

namespace Game
  export
  leftShift : Game a -> Game a
  leftShift (MkGame g) = MkGame (map leftShift g)

  export
  rightShift : Game a -> Game a
  rightShift (MkGame g) = MkGame (map rightShift g)

  export
  upShift : Game a -> Game a
  upShift (MkGame g) = MkGame (leftShift g)

  export
  downShift : Game a -> Game a
  downShift (MkGame g) = MkGame (rightShift g)

export
Comonad Game where
  extract (MkGame g) = extract $ extract g
  duplicate g = let il = iterate leftShift (leftShift g)
                    ir = iterate rightShift (rightShift g)
                    inner = MkZipper il g ir
                    ol = iterate (map upShift) (map upShift inner)
                    or = iterate (map downShift) (map downShift inner) in
                    MkGame (MkZipper ol inner or)

public export
data Cell = Alive | Dead

export
Show Cell where
  show Alive = "X"
  show Dead = "O"

export
isAlive : Cell -> Bool
isAlive Alive = True
isAlive Dead = False

export
isDead : Cell -> Bool
isDead Alive = False
isDead Dead = True

aliveNeighbours : Game Cell -> Nat
aliveNeighbours g = let tl = extract . upShift . leftShift $ g
                        tc = extract . upShift $ g
                        tr = extract . upShift . rightShift $ g
                        cl = extract . leftShift $ g
                        cr = extract . rightShift $ g
                        bl = extract . downShift . leftShift $ g
                        bc = extract . downShift $ g
                        br = extract . downShift . rightShift $ g in
                        sum $ map toNat [tl, tc, tr, cl, cr, bl, bc, br]
  where toNat : Cell -> Nat
        toNat Alive = 1
        toNat Dead = 0

stepCell : Game Cell -> Cell
stepCell g = let cell = extract g
                 ns = aliveNeighbours g in
                 if ns > 3 || ns < 2 then Dead
                    else if isDead cell && ns == 3 then Alive
                            else cell

export
nextStep : Game Cell -> Game Cell
nextStep = extend stepCell

export
example : Game Cell
example = let deadRow = MkZipper (repeat Dead) Dead (repeat Dead)
              topRow = MkZipper (repeat Dead) Alive (repeat Dead)
              centerRow = MkZipper (repeat Dead) Dead (Alive :: repeat Dead)
              bottomRow = MkZipper (Alive :: repeat Dead) Alive (Alive :: repeat Dead) in
              MkGame (MkZipper (topRow :: repeat deadRow) centerRow (bottomRow :: repeat deadRow))

gameLine : Nat -> Nat -> Zipper Cell -> List (Nat, Nat, Cell)
gameLine k r (MkZipper before focus after) =
  let b = reverse $ take k $ zip3 (repeat r) (iterate (`minus` 1) (k `minus` 1)) before
      a = take k $ zip3 (repeat r) (iterate S (k + 1)) after in
      b ++ (r, k, focus) :: a

export
gameSquare : Nat -> Game Cell -> List (Nat, Nat, Cell)
gameSquare k (MkGame (MkZipper before focus after)) =
  let b = reverse $ take k $ zipWith (\i, r => gameLine k i r) (iterate (`minus` 1) (k `minus` 1)) before
      a = take k $ zipWith (\i, r => gameLine k i r) (iterate S (k + 1)) after in
      concat $ b ++ (gameLine k k focus) :: a

export
showZipperR : Show a => Nat -> Zipper a -> String
showZipperR k (MkZipper before focus after) =
  let l = concat $ map show $ reverse $ take k before
      r = concat $ map show $ take k after in
      l ++ show focus ++ r

export
showGameR : Nat -> Game Cell -> String
showGameR k (MkGame (MkZipper top center bottom)) =
  let t = map (showZipperR k) (reverse $ take k top)
      b = map (showZipperR k) (take k bottom) in
      unlines $ t ++ [showZipperR k center] ++ b

compactZipper : Nat -> Zipper Cell -> Zipper Cell
compactZipper k (MkZipper ls f rs) =
  MkZipper (fromList (take k ls) (repeat Dead))
           f
           (fromList (take k rs) (repeat Dead))

export
compact : Nat -> Game Cell -> Game Cell
compact k (MkGame (MkZipper top mid bot)) =
  let deadRow = MkZipper (repeat Dead) Dead (repeat Dead) in
      MkGame (MkZipper (fromList (take k top) (repeat deadRow))
                       (compactZipper k mid)
                       (fromList (take k bot) (repeat deadRow)))
