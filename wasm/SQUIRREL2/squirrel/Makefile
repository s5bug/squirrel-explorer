SQUIRREL= ..


OUT= $(SQUIRREL)/lib/libsquirrel.a
INCZ= -I$(SQUIRREL)/include -I. -Iinclude
DEFS= 
LIB=	

OBJS= \
	sqapi.o \
	sqbaselib.o \
	sqcompiler.o \
	sqdebug.o \
	sqlexer.o \
	sqobject.o \
	sqparser.o \
	sqstate.o \
	sqtable.o \
	sqvm.o \
	sqmem.o \
	sqclass.o
	
SRCS= \
	sqapi.cpp \
	sqbaselib.cpp \
	sqfuncstate.cpp \
	sqdebug.cpp \
	sqlexer.cpp \
	sqobject.cpp \
	sqcompiler.cpp \
	sqstate.cpp \
	sqtable.cpp \
	sqmem.cpp \
	sqvm.cpp \
	sqclass.cpp

	
	
sq32:
	$(CXX) $(CXXFLAGS) -c $(SRCS) $(INCZ) $(DEFS)
	mkdir -p $(dir $(OUT))
	$(AR) rc $(OUT) *.o
	rm *.o

sqprof:
	$(CXX) $(CXXFLAGS) -pg -pie -gstabs -g3 -c $(SRCS) $(INCZ) $(DEFS)
	mkdir -p $(dir $(OUT))
	$(AR) rc $(OUT) *.o
	rm *.o

sq64:
	$(CXX) $(CXXFLAGS) -D_SQ64 -c $(SRCS) $(INCZ) $(DEFS)
	mkdir -p $(dir $(OUT))
	$(AR) rc $(OUT) *.o
	rm *.o

.PHONY: clean
clean:
	rm -f $(OUT)
