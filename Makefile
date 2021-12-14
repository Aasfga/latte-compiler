all:
	ghc -isrc/ app/Main.hs -outputdir bin -o latte 

package:
	tar -czvf latte-compiler.tar.gz app src test latc_x86_64 Makefile stack.yaml stack.yaml.lock package.yaml

clean:
	rm -r ./bin/
	rm latte-compiler.tar.gz
	rm ./latte

.PHONY: clean package