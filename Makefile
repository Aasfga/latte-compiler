all:
	ghc -isrc/ app/Main.hs -outputdir bin -o latte 

clean:
	rm -r ./bin/
	rm ./latte%