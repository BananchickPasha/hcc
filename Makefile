CC=ghc
TARG=hcc
OBJS= $(FILES:.hs=.o)
HIES= $(FILES:.hs=.hi)
SHIT= $(FILES:.hs=.dyn*)
FLAGS= -Wall\
			 -threaded\
			 -ddump-splices\
			 -dynamic
build: $(FILES)
	$(CC) Main.hs $(FLAGS) -outputdir bin -o "bin/$(TARG)"

clean:
	rm -rf bin/*
