#
#
#

SR_DIR = $$(SOLVER_SRCDIR)/send_recv
SRC_SR = $(shell ls *.f90)
MOD_SR = $(addsuffix .o,$(basename $(SRC_SR)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SR_DIR =  $(SR_DIR)'       >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SR=  \\ >> $(MAKENAME)
	@echo $(MOD_SR) >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SR_DIR)' $(SRC_SR)
