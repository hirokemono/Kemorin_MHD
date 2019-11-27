#
#
#

SPH_COMMDIR = $$(PARA_SRCDIR)/SPH_SHELL_src
SRC_COMM_SPH = $(shell ls *.f90 *.F90)
MOD_COMM_SPH = $(addsuffix .o,$(basename $(SRC_COMM_SPH)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_COMMDIR = $(SPH_COMMDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_COMM_SPH)' >> $(MAKENAME)


mod_list:
	@echo  MOD_COMM_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMM_SPH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_COMMDIR)' $(SRC_COMM_SPH)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
