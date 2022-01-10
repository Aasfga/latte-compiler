all:
	happy src/Parser/BnfcParser/ParLatte.y
	alex src/Parser/BnfcParser/LexLatte.x
	ghc -isrc/ app/Main.hs -outputdir bin -o latte -package microlens-platform -package mtl
	gcc -c lib/runtime.c -o lib/runtime.o

package:
	tar -czvf latte-compiler.tar.gz app lib src test latc_x86_64 Makefile stack.yaml stack.yaml.lock package.yaml README.md

generate-parser:
	bnfc --haskell --functor -o src -p "Parser.BnfcParser" resources/latte.cf
	rm src/Parser/BnfcParser/TestLatte.hs
	
clean:
	rm -r ./bin/
	rm latte-compiler.tar.gz
	rm ./latte
	rm -r src/Parser/BnfcParser

.PHONY: clean package generate-parser