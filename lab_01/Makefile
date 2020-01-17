CC := ghc
MAIN := Main.hs
APPNAME := app.exe

build:
	$(CC) -o $(APPNAME) --make $(MAIN)

clean:
	rm *.o *.hi *.exe *.out

run: build
	./$(APPNAME)
