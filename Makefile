perfect: perfect.hs Perfect/*.hs
	ghc -O2 -threaded perfect.hs

all: perfect

clean:
	rm *.hi *.o */*.hi */*.o perfect
