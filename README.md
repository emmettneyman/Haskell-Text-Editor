# Hexto: A Terminal Text Editor in Haskell
Created by
- Alex Goodisman (gooalex)
- Emmett Neyman (eneyman)
- Aspyn Palatnick (aspyn)

Our editor relies on two external libraries: [Brick](https://hackage.haskell.org/package/brick) and [Vty](http://hackage.haskell.org/package/vty).

To build our project do
`cabal new-build`

And to run our project do
`cabal new-exec hexto <path/to/file.txt>`

## Project Components (In Order)

### PieceTable.hs
This module contains the backing data structure of our text editor: the piece table. The piece table can be thought of as a list of *edits* where each *edit* describes how the text should be modified. This module not only implements this data structure, but also provides an interface for using it.
### EditorState.hs
This module contains the all the code dealing with our editor's state. While the piece table keeps track of the edits we've made so far, the EState data structure maintains other pieces of state such as cursor position, any highlighted text, and the associated file pointer. The majority of this file deals with making sure that the cursor is correctly displayed and that the state of the piece table is always consistent with what is drawn to the screen.
### EventHandler.hs
This module deals with processing events from the user and updating state accordingly. Events include inserting characters, highlighting text, redo-ing/undo-ing edits, copying/pasting text, deleting characters, saving the file, scrolling, moving the cursor around, and exiting the editor. This module also contains code that deals with scrolling the entire editor's view when the cursor would go offscreen.
### Hexto.hs
This module ties the entire project together by calling the Brick library functions needed to create the widgets displayed to the screen. This file defines the draw function that extracts the text out of the editor's state and displays it on screen. It also parses the command line argument and loads the file's initial contents into the editor's state.