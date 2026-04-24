#
#
#

SR_DIR = $$(SOLVER_SRCDIR)/send_recv
SOURCES = $(shell ls *.f90)
MOD_SR = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SR_DIR =  $(SR_DIR)'       >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$(MOD_SR)' >> $(MAKENAME)
	@echo 'MOD_SR=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_SR)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SR_DIR)' $(SOURCES)
