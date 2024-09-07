SQUIRREL= ..


OUT= $(SQUIRREL)/bin/sq
INCZ= -I$(SQUIRREL)/include -I. -I$(SQUIRREL)/sqlibs
LIBZ= -L$(SQUIRREL)/lib 
LIB= -lsquirrel -lsqstdlib

OBJS= sq.o
	
SRCS= sq.c
	
	
sq32:
	$(CXX) $(CXXFLAGS) -o $(OUT) $(SRCS) $(INCZ) $(LIBZ) $(LIB)

sqprof:
	$(CXX) $(CXXFLAGS) -pg -pie -gstabs -g3 -o $(OUT) $(SRCS) $(INCZ) $(LIBZ) $(LIB)
	
sq64:
	$(CXX) $(CXXFLAGS) -D_SQ64 -o $(OUT) $(SRCS) $(INCZ) $(LIBZ) $(LIB)

.PHONY: clean
clean:
	rm -f $(OUT)
