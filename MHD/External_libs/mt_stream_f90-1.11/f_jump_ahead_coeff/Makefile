INCLUDES = -I../

all : f_jump_coeff check

f_jump_coeff : f_jump_coeff.o f_get_coeff.o gf2xe.o ../mt_kind_defs.o
	$(FC) $(LDFLAGS) $^ -o $@

f_jump_coeff.o : f_jump_coeff.F90 ../mt_kind_defs.mod
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

f_get_coeff.o : f_get_coeff.F90 gf2xe.o ../mt_kind_defs.mod
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

gf2xe.o : gf2xe.F90 ../mt_kind_defs.mod
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

../mt_kind_defs.mod : ../mt_kind_defs.F90
../mt_kind_defs.o : ../mt_kind_defs.F90

check : sample.output check.output
	@sh ./check.sh

check.output : f_jump_coeff
	./f_jump_coeff > check.output
	
clean :
	rm -f *.o *~ *bak *mod f_jump_coeff check.output
