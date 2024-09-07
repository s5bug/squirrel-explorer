SQUIRREL= ..


OUT= $(SQUIRREL)/lib/libsqstdlib.a
INCZ= -I$(SQUIRREL)/include -I. -Iinclude

SRCS= \
	sqstdblob.cpp \
	sqstdio.cpp \
	sqstdstream.cpp \
	sqstdmath.cpp \
	sqstdsystem.cpp \
	sqstdstring.cpp \
	sqstdaux.cpp \
	sqstdrex.cpp
	
	
sq32:
	$(CXX) $(CXXFLAGS) -c $(SRCS) $(INCZ)
	$(AR) rc $(OUT) *.o
	rm *.o

sqprof:
	$(CXX) $(CXXFLAGS) -pg -pie -gstabs -g3 -c $(SRCS) $(INCZ)
	$(AR) rc $(OUT) *.o
	rm *.o
sq64:
	$(CXX) $(CXXFLAGS) -D_SQ64 -c $(SRCS) $(INCZ)
	$(AR) rc $(OUT) *.o
	rm *.o

.PHONY: clean
clean:
	rm -f $(OUT)
