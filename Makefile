perfect: perfect.hs Perfect/*.hs
	ghc -O2 -threaded -Wall perfect.hs

testPerfect: testPerfect.hs Perfect/*.hs
	ghc -O2 -Wall testPerfect.hs

all: perfect testPerfect
	./testPerfect

clean:
	rm *.hi *.o */*.hi */*.o perfect testPerfect
