perfect: perfect.hs
	ghc -O2 -threaded perfect.hs

perfect.txt: perfect
	./perfect +RTS -s -N2

all: perfect

clean:
	rm *.hi *.o */*.hi */*.o perfect
