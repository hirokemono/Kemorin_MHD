#
#
#

SR_DIR = $$(PARA_SRCDIR)/rms

LIB_SR = send_recv
SRC_SR = $(shell ls *.f90)
MOD_SR = $(addsuffix .o,$(basename $(SRC_SR)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SR_DIR = $(SR_DIR)'  >> $(MAKENAME)


lib_name:
	@echo  'LIB_SR = ' $(LIB_SR) >> $(MAKENAME)
	@echo  'LIB_SR_FILE = lib$$(LIB_SR).a' \
	>> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB)' '$$@'  >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_SR_FILE): $$(MOD_SR)' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SR)' >> $(MAKENAME)


mod_list:
	@echo MOD_SR=  \\ >> $(MAKENAME)
	@echo $(MOD_SR) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SR_DIR)' $(SRC_SR)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
