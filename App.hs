module Main where

import Brick
import Brick.BChan
import Brick.Main as M
import Brick.Markup (Markup, markup)
import Brick.Util
import Brick.Widgets.Center
import Brick.Widgets.Border (border, borderAttr, borderWithLabel)
import Brick.Widgets.Border.Style
import Data.Text (pack)
import Data.Text.Markup (fromText, markupSet)

import qualified Graphics.Vty as V

import EventHandler (handleEvent, getVP, Name)
import PieceTable
import EditorState (getScrollDelta, getCursorPos, 
                    computeDrawPos, initialState,
                    EState, getPieceTable, getFileContent,
                    getPTForDraw)

import System.IO
import System.Environment
import System.Directory


-- brick needs this to be definedL
app :: M.App EState e Name
app = App { appDraw = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const $ attrMap V.defAttr [(attrName "highlighted", bg V.blue)]
          }

generatePageWidgetWithHighlight :: EState -> Widget Name
generatePageWidgetWithHighlight s = -- let pt = getPTForDraw s in
                                    let text = flush (getPieceTable s) (getFileContent s) in
                                    let pt = getPieceTable s in
                                    case getHLRange pt of
                                        Nothing          -> txtWrap $ pack $ text
                                        Just (_, 0)      -> txtWrap $ pack $ text
                                        Just (pos, len)  -> 
                                            markup markedUpTxt where 
                                                initialTxt :: Markup AttrName
                                                initialTxt = fromText (pack text)
                                                markedUpTxt :: Markup AttrName
                                                markedUpTxt = markupSet (pos, len) (attrName "highlighted") initialTxt

draw :: EState -> [Widget Name]
draw s = pure p where
    p = Brick.padAll 1
        $ vLimit 20
        $ Brick.showCursor getVP (Brick.Location $ 
            computeDrawPos (getScrollDelta s) (getCursorPos s)) 
        $ viewport getVP Vertical 
        $ generatePageWidgetWithHighlight s


main :: IO ()
main = do
    args <- getArgs
    if length args > 0 then
        let filePath = (args !! 0) in
        do
            fileExists <- doesFileExist filePath
            if fileExists then
                do
                    handle <- openFile filePath ReadMode  
                    contents <- hGetContents handle  
                    run contents filePath
                    hClose handle
            else
                do
                    writeFile filePath ""
                    run "" filePath
    else putStrLn "Error: App requires argument <file path> to run"

run :: String -> FilePath -> IO ()
run s fp = do
    eventChan <- Brick.BChan.newBChan 10
    let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
    initialVty <- buildVty
    _ <- Brick.customMain initialVty buildVty (Just eventChan) app $ initialState s fp
    return ()
