all : jump_coeff check

jump_coeff : jump_coeff.o get_coeff.o
	$(CXX) $(LDFLAGS) $^ -o $@ $(LIBS) 

jump_coeff.o : jump_coeff.cxx
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $^

get_coeff.o : get_coeff.cxx
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $^

check : sample.output check.output
	@sh ./check.sh

check.output : jump_coeff
	./jump_coeff > check.output
	
clean :
	rm -f *.o *~ *bak jump_coeff check.output
