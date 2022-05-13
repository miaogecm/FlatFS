CXXFLAGS := -O2 -Wall -MD # -g -fno-omit-frame-pointer
#CXXFLAGS += -I$(LDB)
#LDFLAGS += -L/Users/kaashoek/hack/db-4.7.25.NC/build_unix/
#LDFLAGS += -pg -lprofiler

OBJS := args.o pedsort.o primes.o 

all: pedsort

primes.C: mkprimes
	./mkprimes > $@

pedsort: $(OBJS)
	g++ -o $@ $^ -ldb -pthread

-include $(OBJS:.o=.d)

.PHONY: clean
clean: 
	rm -f pedsort primes.C $(OBJS) $(OBJS:.o=.d)
