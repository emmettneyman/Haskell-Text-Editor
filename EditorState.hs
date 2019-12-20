module EditorState (EState, initialState, handleEdit, getCursorPos, getPieceTable, 
                    getFilePath, handleCursorMove, moveCursorAfterUndoRedo,
                    handleMouseUp, handleMouseDown, cursorPosToSingleIndexHelper,
                    checkCursorPos, getFileContent, getScrollDelta, handleScroll,
                    linesInDoc, computeDrawPos, lineHasOneChar, setScrollDelt) where

import PieceTable

import Control.Monad (when)
import Control.Monad.IO.Class
import Graphics.Vty as V
import System.Exit (exitFailure)

-- EState stores the state of the editor:
--      where the cursor is drawn
--      what the current Piece Table is
--      what range is currently highlighted
--      whether the mouse is currently being clicked
data EState = ES { cursorPos :: (Int, Int), pieceTable :: PieceTable,
                   highlightRange :: ((Int, Int), (Int, Int)),
                   mouseDown :: Bool, filePath :: FilePath,
                   fileContent :: String, deltaScroll :: Int } deriving Show

-- Initial state of the editor
initialState :: String -> FilePath -> EState
initialState initialContents fp = ES { cursorPos = (0, 0),
                    pieceTable = empty,
                    highlightRange = ((0, 0), (0,0)),
                    mouseDown = False,
                    filePath = fp,
                    deltaScroll = 0,
                    fileContent = initialContents }

-- Returns the current cursor position
getCursorPos :: EState -> (Int, Int)
getCursorPos = cursorPos

-- Returns the current Piece Table
getPieceTable :: EState -> PieceTable
getPieceTable = pieceTable

-- Returns the file path
getFilePath :: EState -> FilePath
getFilePath = filePath

-- Returns the file content
getFileContent :: EState -> String
getFileContent = fileContent

-- Returns the scrolling delta
getScrollDelta :: EState -> Int
getScrollDelta = deltaScroll

setScrollDelt :: EState -> Int -> EState
setScrollDelt es  i = ES { cursorPos = cursorPos es,
                           pieceTable = pieceTable es,
                           highlightRange = highlightRange es,
                           mouseDown = mouseDown es,
                           filePath = filePath es,
                           deltaScroll = 0,
                           fileContent = fileContent es }

-- update the current Piece Table
handleEdit :: EState -> PieceTable -> EState
handleEdit es pt = ES { cursorPos = newPos,
                        pieceTable = pt,
                        highlightRange = highlightRange es,
                        mouseDown = False,
                        filePath = filePath es,
                        deltaScroll = deltaScroll es,
                        fileContent = fileContent es } where
                            newPos = checkCursorPos pt (fileContent es) (x, y) where
                                (x, y) = convertCursorToXY pt (fileContent es) $ getCursorPosPT pt

-- update the scroll delta to track scrolling
handleScroll :: EState -> Int -> EState
handleScroll es i = ES { cursorPos = cursorPos es,
                         pieceTable = pieceTable es,
                         highlightRange = highlightRange es,
                         mouseDown = False,
                         filePath = filePath es,
                         deltaScroll = i + deltaScroll es,
                         fileContent = fileContent es }


-- Set the cursor position based on the piece table's cursor position
moveCursorAfterUndoRedo :: EState -> EState
moveCursorAfterUndoRedo es = handleEdit es (pieceTable es)

-- Move the cursor by the given tuple
handleCursorMove :: EState -> (Int, Int) -> EState
handleCursorMove es i =
    ES { cursorPos = newPos,
         pieceTable = newPT,
         highlightRange = highlightRange es,
         mouseDown = False,
         filePath = filePath es,
         deltaScroll = deltaScroll es,
         fileContent = fileContent es } where    
            newPos = checkCursorPos (pieceTable es) (fileContent es)
                (fst (cursorPos es) + fst i, snd (cursorPos es) + snd i)
            newPT = moveTo (cursorPosToSingleIndex newPos es) (pieceTable es)

-- handles a mouse click event
handleMouseDown :: EState -> (Int, Int) -> EState
handleMouseDown es l@(x, y) = if mouseDown es then updateCursor else updateHL where
    updateHL = 
        ES { cursorPos = cursorPos es,
            pieceTable = pieceTable es,
            highlightRange = newHLRange,
            mouseDown = True,
            filePath = filePath es,
            deltaScroll = deltaScroll es,
            fileContent = fileContent es } where
                newHLRange = ((x, y), l)
    updateCursor = 
        ES { cursorPos = l,
            pieceTable = newPT,
            highlightRange = newHLRange,
            mouseDown = True,
            filePath = filePath es,
            deltaScroll = deltaScroll es,
            fileContent = fileContent es } where
                newHLRange = ((x', y'), l) where
                    (x', y') = fst $ highlightRange es
                -- if end pos is less than start pos, len will be negative
                newPT = if len < 0 then highlight (pos + len) (-len) (pieceTable es) 
                                   else highlight pos len (pieceTable es) where
                    pos = cursorPosToSingleIndex (fst newHLRange) es
                    len = cursorPosToSingleIndex (snd newHLRange) es - pos

-- handles a mouse unclick event
handleMouseUp :: EState -> (Int, Int) -> EState
handleMouseUp es l@(x, y) =
    ES { cursorPos = l,
         pieceTable = newPT,
         highlightRange = newHLRange,
         mouseDown = False,
         filePath = filePath es,
         deltaScroll = deltaScroll es,
         fileContent = fileContent es } where
            newHLRange = ((x', y'), l) where
                (x', y') = fst $ highlightRange es
            -- if end pos is less than start pos, len will be negative
            newPT = if len < 0 then highlight (pos + len) (-len) (pieceTable es) 
                               else highlight pos len (pieceTable es) where
                pos = cursorPosToSingleIndex (fst newHLRange) es
                len = cursorPosToSingleIndex (snd newHLRange) es - pos


-- Convert a tuple cursor position to a single index
cursorPosToSingleIndex :: (Int, Int) -> EState -> Int
cursorPosToSingleIndex cursorPos es =
    let s = flush (pieceTable es) (fileContent es) in
    cursorPosToSingleIndexHelper cursorPos s
        

-- Useful for debugging because has a string
-- Given a cursor position and a string, calculate the "absolute"
-- position of the cursor    
cursorPosToSingleIndexHelper :: (Int, Int) -> String -> Int
cursorPosToSingleIndexHelper cursorPos s = 
    getIndex cursorPos s 0 where
    getIndex (charNum, lineNum) s' currIdx =
        if lineNum == 0 && charNum == 0 then currIdx 
        else
            case s' of
                h : tl
                    | h == '\n' -> getIndex (charNum, lineNum - 1) tl (currIdx + 1)
                    | lineNum == 0 -> getIndex (charNum - 1, lineNum) tl (currIdx + 1)
                    | otherwise -> getIndex (charNum, lineNum) tl (currIdx + 1)
                []     -> currIdx

-- Get the index of the last character in the given line
lastCharIndexInLine :: Int -> EState -> Int
lastCharIndexInLine lineNum es =
    let s = flush (pieceTable es) (fileContent es) in
    lastCharIndexInLineHelper lineNum s

lastCharIndexInLineHelper :: Int -> String -> Int
lastCharIndexInLineHelper lineNum s = 
    getIndex lineNum s 0 where
    getIndex lineNum' s' currIdx =
        if lineNum' == 0 then 
            case s' of
                h : tl -> if h == '\n' then currIdx else getIndex lineNum' tl (currIdx + 1)
                []     -> currIdx
        else
            case s' of
                h : tl -> if h == '\n' 
                          then getIndex (lineNum' - 1) tl currIdx
                          else getIndex lineNum' tl currIdx
                []     -> currIdx -- TODO this part may have a bug


-- Count the number of lines in a string, where the minimum
-- number of lines is 1 if there is any text
countNewLines :: String -> Int
countNewLines s = if s == "" then 0 else 1 + length (filter (== '\n') s)

-- Get the number of lines in the document, given the editor state
linesInDoc :: EState -> Int
linesInDoc es = countNewLines (flush (pieceTable es) (fileContent es))

-- Takes in a cursor position and a state and returns a new cursor position
-- that represents an adjusted version of the original position such that the
-- position is now at a valid location in the editor 
checkCursorPos :: PieceTable -> String -> (Int, Int) -> (Int, Int)
checkCursorPos pt s = 
    checkCursorPosHelper (flush pt s)

checkCursorPosHelper :: String -> (Int, Int) -> (Int, Int)
checkCursorPosHelper s (0, 0) = (0, 0)
checkCursorPosHelper "" _ = (0, 0)
checkCursorPosHelper s (charNum, lineNum) =
    let numLines = countNewLines s in
    let adjLineNum
            | lineNum < 0 = 0
            | lineNum >= numLines = numLines - 1
            | otherwise = lineNum in
    -- Lengths of current line we are trying to move to
    let lineLength
            | length (lines s) /= numLines && adjLineNum == numLines = 0
            | length (lines s) /= numLines && adjLineNum == length (lines s) = 0
            | length (lines s) <= adjLineNum = length (lines s !! (adjLineNum - 1))
            | otherwise = length (lines s !! adjLineNum) in
    -- Cases
    -- 1) Cursor is below bottom of editor --> move cursor up one line
    -- 2) Cursor is above top of editor --> move cursor down one line
    -- 3) Cursor is to the left of beginning of line --> move cursor right one char
    -- 4) Cursor is to the right of end of line --> move cursor left one char
    let updatedCharNum
            | charNum < 0 = 0
            | charNum > lineLength = lineLength
            | otherwise = charNum in
    let updatedLineNum
            | adjLineNum < 0 = 0
            | adjLineNum >= numLines = numLines - 1
            | otherwise = adjLineNum in
    (updatedCharNum, updatedLineNum)

-- Convert a cursor index to a (character, line number)
convertCursorToXY :: PieceTable -> String -> Int -> (Int, Int)
convertCursorToXY pt s =
    convertCursorToXYHelper (flush pt s)

convertCursorToXYHelper :: String -> Int -> (Int, Int)
convertCursorToXYHelper s cp =
    foldl (\(x, y) c -> if c == '\n' then (0, y + 1) else (x + 1, y)) (0,0) (take cp s)

-- Given the scroll delta and the document's cursor position, calculate
-- where on the screen the cursor should be drawn
computeDrawPos :: Int -> (Int, Int) -> (Int, Int)
computeDrawPos sd (x, y) = (x, y - sd)

-- Helper method to see if a given line has only one character
lineHasOneChar :: EState -> Int -> Bool
lineHasOneChar es i =
    let s = flush (pieceTable es) (fileContent es) in
    let l = lines s in
        ((length l > i) && (let l' = l !! i in length l' == 1))