all: Hexto.hs EventHandler.hs EditorState.hs PieceTable.hs clean
	ghc -threaded Hexto.hs -o hexto

clean:
	rm -rf hexto *.hi *.o
