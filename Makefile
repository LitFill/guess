build: app/Main.hs
	@cabal build -v0

run: build 
	@cabal run
