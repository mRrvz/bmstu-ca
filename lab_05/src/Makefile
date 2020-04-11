CC := ghc
MAIN := Main.hs
APPNAME := app.exe

build:
	$(CC) -o $(APPNAME) --make $(MAIN)

table:
	$(CC) -o gen.exe  --make TableGen.hs

clean:
	rm *.o *.hi *.exe *.out

run: build
	./$(APPNAME)
