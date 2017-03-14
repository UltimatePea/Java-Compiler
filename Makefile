identity: Tokenizer.o IdentityCompile.o
	ghc -o IdentityCompile Tokenizer.o IdentityCompile.o
	
IdentityCompile.o: IdentityCompile.hs
	ghc -c IdentityCompile.hs

Tokenizer.o : Tokenizer.hs
	ghc -c Tokenizer.hs

clean:
	rm -rf *.o
	rm -rf *.hi
	rm -rf IdentityCompile
	
