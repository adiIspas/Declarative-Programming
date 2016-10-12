-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck
import Data.Char



-- Exercise 8:

pic1 :: Picture
pic1 = beside knight (beside (invert knight) (beside whiteSquare (beside (flipV (flipV knight)) (flipV (flipV (invert knight))))))

pic2 :: Picture
pic2 = beside knight (beside (invert knight) (beside whiteSquare (beside (flipV knight) (flipV (invert knight)))))


-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

whiteRow :: Picture
whiteRow = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))

whitePawns :: Picture
whitePawns = repeatH 8 pawn

blackRow :: Picture
blackRow = invert whiteRow

blackPawns :: Picture
blackPawns = invert whitePawns

whiteBoard :: Picture
whiteBoard = over (above whiteRow whitePawns) (above emptyRow otherEmptyRow)

blackBoard :: Picture
blackBoard = over (above blackPawns blackRow) (above emptyRow otherEmptyRow)

-- e)

populatedBoard :: Picture
populatedBoard = above whiteBoard (above middleBoard blackBoard)


-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 10:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = above (twoBeside x) (invert (twoBeside x))

-- Exercise 11 (extra credit):
-- https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

emptyBoard :: Picture
emptyBoard = above middleBoard middleBoard

-- Using the "clear" Picture to skip over the board
mate :: Picture
mate = over (above clear (above (beside (repeatH 6 clear) (beside (invert queen) king)) (beside (repeatH 5 clear) (invert king)))) middleBoard

black_Pawns_7 :: Picture
black_Pawns_7 = undefined

e4c5Nf3 :: Picture
e4c5Nf3 = undefined
--- "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R"
