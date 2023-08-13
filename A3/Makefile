CXX = g++-8
EXE = iplC
CXXDEBUG = -g -Wall
CXXSTD = -std=c++11


.PHONY: all clean

all: type.o ast.o symbtab.o parser.o scanner.o main.o
	$(CXX) $(CXXSTD) $(CXXDEBUG) -o $(EXE) main.cpp parser.o scanner.o type.o ast.o symbtab.o

main.o: main.cpp
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c $< -o $@

parser.o: parser.yy scanner.hh
	bison -d -v $<
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c parser.tab.cc -o $@ 

scanner.o: scanner.l scanner.hh parser.tab.hh parser.tab.cc	
	flex++ --outfile=scanner.yy.cc  $<
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c scanner.yy.cc -o $@

ast.o: ast.cpp ast.hh
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c $< -o $@

type.o: type.cpp type.hh
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c $< -o $@

symbtab.o: symbtab.cpp symbtab.hh
	$(CXX) $(CXXSTD) $(CXXDEBUG) -c $< -o $@

clean:
	rm -f *.o iplC location.hh position.hh stack.hh parser.output parser.tab.cc parser.tab.hh scanner.yy.cc a.out tmp.s