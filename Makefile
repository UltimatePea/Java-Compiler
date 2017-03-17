

cabal:
	cabal build

analyzer: JavaAnalyzer.o bin
	ghc -o bin/JavaAnalyzer Tokenizer.o JavaAnalyzer.o
	./bin/JavaAnalyzer

identity: IdentityCompile.o bin
	ghc -o bin/IdentityCompile Tokenizer.o IdentityCompile.o
	./bin/Tokenizer

bin:
	mkdir bin
JavaAnalyzer.o: Tokenizer.o
IdentityCompile.o: Tokenizer.o

.SUFFIXES: .o .hs

.hs.o:
	ghc -c $<
	

clean:
	rm -rf *.o
	rm -rf *.hi
	rm -rf bin/
	
