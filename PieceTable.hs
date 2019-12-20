module PieceTable  (toPiece, PieceTable, moveTo, addChar, delChar, flush,
                    undo,redo, highlight, copy, paste, empty,
                    getCursorPosPT, getHLRange) where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)

data Edit = E {text :: String, start :: Int, end :: Int} deriving Show

newtype Piece = P {doPiece :: String -> String}

toPiece :: Edit -> Piece
toPiece (E text start end) = P (\s -> take start s ++ reverse text ++ drop end s)

--the current operations that are actually used are
--moveto, addchar, and delchar
--as such the text is reversed
--to allow add/remove char at the current text pointer
--which is the list head
--this has the unfortunate side effect of making
--replace and friends have the text backwards
--so we have to preemptively reverse so it ends up correct again after being flusheds

instance Semigroup Piece where
    (P f1) <> (P f2) = P (f1 . f2)

instance Monoid Piece where
    mempty = P id

data PieceTable = PT {current :: Either Edit (Int, Int), past :: [Edit], future :: [Edit], clipboard :: String} deriving Show


--operate on the blob and produce the thing we will put on the screen
flush :: PieceTable -> String -> String
flush = doPiece . foldMap toPiece . past . moveTo 0 --moveTo 0 is a bit of a hack But
-- the point is to lock the current edit, then get the past ones (including the one we just locked)
--so that we can fold using monoid and get our final string->string function
-- and then operate on a string


--cursor position
getCursorPosPT :: PieceTable -> Int
getCursorPosPT pt = case current pt of
    Right (pos, len)       -> pos + len
    Left (E txt start end) -> start + length txt

--highlight position
getHLRange :: PieceTable -> Maybe (Int, Int)
getHLRange pt = case current pt of
    Right (pos, len)       -> Just (pos, len)
    Left (E txt start end) -> Nothing


--create a new zero length highlight for future edits
moveTo :: Int -> PieceTable -> PieceTable
moveTo c (PT (Right _) p f cb) = PT (Right (c,0)) p f cb
moveTo c (PT (Left e) p f cb) = PT (Right (c,0)) (e:p) f cb

--create onechar edit or add char to current edit
addChar :: Char -> PieceTable -> PieceTable
addChar c (PT (Right (i,s)) p f cb) = PT (Left (E [c] i (i+s))) p f cb
addChar c (PT (Left(E t s e)) past future cb) = PT (Left(E (c:t) s e)) past future cb

--remove char from current edit or create new edit deleting characters
delChar :: PieceTable -> PieceTable
delChar (PT (Right (0,0)) p f cb) = PT (Right (0,0)) p f  cb--delete cursor at start
delChar (PT (Right (i,0)) p f cb) = PT (Left (E "" (i-1) i)) p f  cb--delete cursor elsewhere
delChar (PT (Right (i,s)) p f cb) = PT (Left (E "" i (i+s))) p f  cb--delete highlighted section
delChar (PT (Left (E "" 0 e)) p f cb) = PT (Left (E "" 0 e)) p f cb
delChar (PT (Left (E "" s e)) p f cb) = PT (Left (E "" (s-1) e)) p f cb
delChar (PT (Left (E (_:t) s e)) p f cb) = PT (Left (E t s e)) p f cb

--for undo and redo, don't store a list of pieces, stores a list of edits
--so we can swap them back in
--and only convert to Pieces when it's time to collapse
undo :: PieceTable -> PieceTable
undo (PT (Right l) [] f cb) = PT (Right l) [] f cb
undo (PT (Left e) [] f cb) = PT (Right (0,0)) [] (e:f) cb
undo (PT (Right _) (h:p) f cb) = PT (Left h) p f cb
undo (PT (Left e) (h:p) f cb) = PT (Left h) p (e:f) cb

--specifically for redo need a forward and backward looking list so that after undo you can put it back
--maybe do whole tree so if you start a new path you can still go to old one
redo :: PieceTable -> PieceTable
redo (PT x p [] cb) = PT x p [] cb
redo (PT (Right _) p (h:f) cb) = PT (Left h) p f cb
redo (PT (Left e) p (h:f) cb) = PT (Left h) (e:p) f cb

--requires having two cursor positions so that it can be come a replacement edit only
--once something is typed
highlight :: Int -> Int -> PieceTable -> PieceTable
highlight pos ct (PT (Right _) p f cb) = PT (Right (pos,ct)) p f cb
highlight pos ct (PT (Left e) p f cb) = PT (Right (pos,ct)) (e:p) f cb

--with highlight, add a clipboard to the piecetable dt
copy :: String -> PieceTable -> PieceTable
copy base pt@(PT (Right (i,s)) p f _) = PT (Right (i,s)) p f (reverse tx) where
    tx = take s . drop i $ flush pt base
copy base pt@(PT (Left e) p f _) = PT (Left e) p f ""

--either insert or replace depending on highlight
paste :: PieceTable -> PieceTable
paste (PT (Right (i,s)) p f cb) = PT (Left (E cb i (i+s))) p f cb
paste (PT (Left(E t s e)) past future cb) = PT (Left(E (cb++t) s e)) past future cb

-- the empty piece table with empty highlight at position 0
empty :: PieceTable
empty = PT (Right (0,0)) [] [] ""


-- example usage
-- > flush (addChar 'c' $ (delChar) $ (moveTo 3) $ (addChar 'a') pt) "bbb"
-- "abcb"


--testing

tType :: Test
tType = "type chars" ~: TestList [
    flush (addChar 'c' . addChar 'b' . addChar 'a' $ empty) "" ~?= "abc",
    flush (addChar 'n' . addChar 'u' $ empty) "happy" ~?= "unhappy"]

tDel :: Test
tDel = "type delete" ~: TestList [
    flush (delChar empty) "begin" ~?= "begin", --del at beginning
    flush (delChar . addChar 'a' $ empty) "type" ~?= "type", --del chars typed
    flush (delChar . addChar 'b' . addChar 'a' $ empty) "gain" ~?= "again", --del some chars typed
    flush (addChar 'c' . delChar . addChar 'f' $ empty) "lose" ~?= "close"] --del then type more

tMove :: Test
tMove = "move cursor" ~: TestList [
    flush (addChar 'r' . moveTo 4 $ empty) "lose" ~?= "loser", --add end
    flush (addChar 't' . addChar 't' . moveTo 2 $ empty) "beer" ~?= "better", --add middle
    flush (delChar . moveTo 6 $ empty) "broken" ~?= "broke", --delete chars already There
    flush (addChar 'r' . addChar 'e' . moveTo 4 . addChar 'r' . addChar 'e' $ empty) "as" ~?= "eraser" ] --add two places

tUndo :: Test
tUndo = "undo" ~: TestList [
    flush (undo . addChar 'f' $ empty) "eat" ~?= "eat",
    flush (addChar 'f' . undo . addChar 's' $ empty) "eat" ~?= "feat",
    flush (undo . addChar 'f' . addChar 's' $ empty) "eat" ~?= "eat", -- undoes a whole contiguous edit
    flush (undo . delChar . moveTo 2 $ empty) "stuff" ~?= "stuff"]

tRedo :: Test
tRedo = "redo" ~: TestList [
    flush (redo . undo . addChar 'f' $ empty) "eat" ~?= "feat",
    flush (redo . redo . undo . undo . addChar 'b' . moveTo 1 . addChar 'c' $ empty) "a" ~?= "cba",
    flush (undo . redo . undo . addChar 'a' $ empty) "b" ~?= "b"]

tHighlight :: Test
tHighlight = "highlight" ~: TestList [
    flush (addChar 's' . highlight 0 2 $ empty) "block" ~?= "sock",
    flush (delChar . highlight 1 1 $ empty) "break" ~?= "beak"]

tCopy :: Test
tCopy = "copypaste" ~: TestList [
    flush (paste . copy "string" . moveTo 1 $ empty) "string" ~?= "string",
    flush (paste . moveTo 1 . copy "string" . highlight 0 3 $ empty) "string" ~?= "sstrtring",
    flush (paste . addChar 'a' . moveTo 1 . copy "string" . highlight 0 3 $ empty) "string" ~?= "sastrtring"]

--sum test
tAll :: Test
tAll = "all" ~: TestList [
    tType, tDel, tMove, tUndo, tRedo, tHighlight, tCopy]
