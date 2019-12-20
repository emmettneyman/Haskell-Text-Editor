module EventHandler (handleEvent, getVP, Name) where

import Brick
import Brick.Main as M
import Graphics.Vty
import Data.Text
import Control.Monad.IO.Class (liftIO)

import EditorState
import PieceTable

-- Datatype for viewports
data Name = VP1 deriving (Ord, Show, Eq)

-- Get the viewport
getVP :: Name
getVP = VP1

-- Viewport that allows scrolling
vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

-- Event handler function that gets invoked by Brick
handleEvent :: EState -> BrickEvent Name e -> EventM Name (Next EState)
handleEvent s (VtyEvent (EvKey key [MCtrl])) =
    case key of
        KChar 'x' -> halt s -- On Ctrl + X, quit
        -- COPY
        KChar 'c' -> let pt = PieceTable.copy "" (getPieceTable s) in
                     let newState = handleEdit s pt in
                     continue newState
        -- PAST
        KChar 'v' -> let pt = PieceTable.paste (getPieceTable s) in
                     let newState = handleEdit s pt in
                     continue newState
        -- REDO
        KChar 'r' -> let pt = redo (getPieceTable s) in
                     let interState = handleEdit s pt in
                     let newState = moveCursorAfterUndoRedo interState in
                     continue newState
        -- UNDO
        KChar 'u' -> let pt = undo (getPieceTable s) in
                     let interState = handleEdit s pt in
                     let newState = moveCursorAfterUndoRedo interState in
                     continue newState
        -- SAVE
        KChar 'w' -> do 
                       liftIO $ writeFile (getFilePath s) (flush (getPieceTable s) "")
                       continue s
        _     -> continue s


handleEvent s (VtyEvent (EvKey key [])) =
    case key of
        KEsc    -> halt s
        KChar c -> let pt = PieceTable.addChar c (getPieceTable s) in
                   let newState = handleEdit s pt in
                   continue newState
        KEnter  -> let posOnScreen = computeDrawPos (getScrollDelta s) (getCursorPos s) in
                   if snd posOnScreen == 19
                   then let pt1 = PieceTable.addChar '\n' (getPieceTable s) in
                        let ns = handleEdit s pt1 in
                        let pt = PieceTable.addChar ' ' pt1 in
                        let ptDel = PieceTable.delChar pt in 
                        let interState = handleEdit ns pt in
                        let newState = handleScroll interState 1 in
                        M.vScrollBy vp1Scroll 1 >> continue newState
                   else let pt = PieceTable.addChar '\n' (getPieceTable s) in
                        let newState = handleEdit s pt in
                        continue newState
        KBS     -> let posOnScreen = computeDrawPos (getScrollDelta s) (getCursorPos s) in
                   if posOnScreen == (0, 0) && getScrollDelta s /= 0
                   then let interState = handleScroll s (-1) in
                        let pt = PieceTable.delChar (getPieceTable s) in
                        let newState = handleEdit interState pt in
                        M.vScrollBy vp1Scroll (-1) >> continue newState
                   else (if
                    (fst posOnScreen == 0 &&
                       linesInDoc s == 20 && (getScrollDelta s /= 0))
                      ||
                      (fst posOnScreen == 1 &&
                         linesInDoc s == 20 && getScrollDelta s /= 0 && lineHasOneChar s 20)
                    then
                    (let interState = setScrollDelt s 0 in
                       let pt = PieceTable.delChar (getPieceTable s) in
                         let newState = handleEdit interState pt in continue newState)
                    else
                    (let pt = PieceTable.delChar (getPieceTable s) in
                       let newState = handleEdit s pt in continue newState))
        KLeft   -> let newState = handleCursorMove s (-1, 0) in
                   continue newState
        KRight  -> let newState = handleCursorMove s (1, 0) in
                   continue newState
        KUp     -> let posOnScreen = computeDrawPos (getScrollDelta s) (getCursorPos s) in
                   if (snd posOnScreen == 0) && (getScrollDelta s /= 0)
                   then let interState = handleScroll s (-1) in
                        let newState = handleCursorMove interState (0, -1) in
                        M.vScrollBy vp1Scroll (-1) >> continue newState
                   else
                      let newState = handleCursorMove s (0, -1) in
                      continue newState
        KDown   -> let posOnScreen = computeDrawPos (getScrollDelta s) (getCursorPos s) in
                    if (snd posOnScreen == 19) &&
                    (linesInDoc s > getScrollDelta s + 20)
                    then let interState = handleScroll s 1 in
                         let newState = handleCursorMove interState (0, 1) in
                         M.vScrollBy vp1Scroll 1 >> continue newState
                    else
                        let newState = handleCursorMove s (0, 1) in
                        continue newState
        _       -> continue s

-- These two cases handle mouse inputs for text highlighting
handleEvent s (MouseDown _ button _ l) = 
    case button of
        BLeft -> let newState = handleMouseDown s $ checkCursorPos (getPieceTable s) (getFileContent s) (loc l) in
                 continue newState
        _     -> continue s

handleEvent s (MouseUp _ button l) =
    case button of
        Just BLeft -> let newState = handleMouseUp s $ checkCursorPos (getPieceTable s) (getFileContent s) (loc l) in
                      continue newState
        _          -> continue s

handleEvent s e = continue s
