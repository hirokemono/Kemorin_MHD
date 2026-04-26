#
#
#

SR_DIR = $$(SOLVER_SRCDIR)/send_recv
SOURCES = $(shell ls *.f90)

MOD_SR = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SR'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SR_DIR =  $(SR_DIR)'       >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_SR)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SR_DIR)' $(SOURCES)
