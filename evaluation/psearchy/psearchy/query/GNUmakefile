LDB := /Users/kaashoek/hack/db-4.7.25.NC/build_unix/
CXXFLAGS := -g -Wall -MD -I$(LDB)

qe_objects := qe.o queryop.o between.o and.o  ../mkdb/args.o

all: qe

qe: $(qe_objects)
	g++ -o qe -L$(LDB) $(qe_objects) -ldb -pthread

.PHONY : clean
clean : 
	rm qe $(qe_objects) 
